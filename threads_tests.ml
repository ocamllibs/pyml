(* Tests of [Py.Thread]: Python threads, OCaml threads and OCaml domains
   calling each other. Built with OCaml 5 on Unix only (see dune).

   Each scenario runs in its own process, with a timeout: most of them
   deadlock (or crash) if the mechanism that they check is missing. Run
   without the environment variable [PYML_THREADS_TEST], the executable runs
   each scenario in a new process (with the same arguments: the version of
   Python); with [PYML_THREADS_TEST=<scenario>], it runs the scenario. *)

(* In seconds; [PYML_THREADS_TIMEOUT] overrides it. *)
let timeout =
  match Sys.getenv_opt "PYML_THREADS_TIMEOUT" with
  | Some t -> float_of_string t
  | None -> 60.

(* Python definitions, in [__main__]. *)
let python_function name code =
  assert (Py.Run.simple_string code);
  Py.Module.get (Py.Module.main ()) name

let set_callback name f =
  let m = Py.Import.add_module "threads_test" in
  Py.Module.set m name (Py.Callable.of_function f)

let check name expected obtained =
  if expected = obtained then Pyml_tests_common.Passed
  else
    Pyml_tests_common.Failed
      (Printf.sprintf "%s: %d expected, %d obtained" name expected obtained)

let spawn_domains n f =
  List.init n (fun i -> Domain.spawn (fun () -> f i)) |> List.map Domain.join

(* Python threads call OCaml: [Py.Thread.call] releases the locks of OCaml
   (otherwise, the Python threads would wait for them), and the threads are
   registered in the OCaml runtime by their first call. *)
let python_threads_call_ocaml () =
  let calls = Atomic.make 0 in
  set_callback "callback" (fun _ ->
    Atomic.incr calls;
    (* Allocates in OCaml and in Python. *)
    Py.Int.of_int (List.length (List.init 100 Fun.id)));
  let run = python_function "run" "
import threading
import threads_test

def run(n, k):
    def work():
        for _ in range(k):
            assert threads_test.callback() == 100
    threads = [threading.Thread(target=work) for _ in range(n)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
" in
  ignore (Py.Thread.call run (Py.Tuple.of_list [Py.Int.of_int 8; Py.Int.of_int 500]));
  check "calls" (8 * 500) (Atomic.get calls)

(* Python's [sum], looked up by the main thread (with the GIL). *)
let python_sum = lazy (
  Py.Module.get_function (Py.Import.import_module "builtins") "sum")

let sum_with_python i =
  let sum = Lazy.force python_sum in
  let total = ref 0 in
  for j = 1 to 200 do
    total := !total + Py.Thread.run_python (fun () ->
      let l = Py.List.of_list_map Py.Int.of_int [i; j] in
      Py.Int.to_int (sum [| l |]))
  done;
  !total

let expected_sums n = List.init n (fun i -> 200 * i + 200 * 201 / 2)

(* OCaml domains call Python. The main thread holds the GIL while it waits
   for the domains in [Domain.join]: the hook of the blocking sections
   releases it. *)
let domains_call_python () =
  let totals =
    Py.Thread.run_ocaml (fun () -> spawn_domains 4 sum_with_python) in
  if totals = expected_sums 4 then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed "wrong sums"

(* The same with OCaml threads (in the domain of the main thread). *)
let ocaml_threads_call_python () =
  let totals = Array.make 4 0 in
  Py.Thread.run_ocaml (fun () ->
    List.init 4 (fun i ->
      Thread.create (fun () -> totals.(i) <- sum_with_python i) ())
    |> List.iter Thread.join);
  if Array.to_list totals = expected_sums 4 then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed "wrong sums"

(* The world lock protects the state of the OCaml code that runs with the
   locks of Python (the state of the libraries that use pyml): the GIL does
   not, since Python code releases it (here, [time.sleep]), and there is no
   GIL with the free-threaded builds. Without the world lock, increments of
   the counter are lost. *)
let world_lock () =
  let counter = ref 0 in
  let sleep = python_function "sleep" "
import time

def sleep():
    time.sleep(0.0001)
" in
  let n = 4 and k = 100 in
  Py.Thread.run_ocaml (fun () ->
    ignore (spawn_domains n (fun _ ->
      for _ = 1 to k do
        Py.Thread.run_python (fun () ->
          let value = !counter in
          ignore (Py.Callable.to_function sleep [| |]);
          counter := value + 1)
      done)));
  check "counter" (n * k) !counter

(* Python threads call OCaml while OCaml domains call Python: the domains
   take the world lock and then the GIL, and the Python threads, which hold
   the GIL, wait for the world lock. They release the GIL while they wait
   (otherwise, the threads deadlock). *)
let python_threads_and_domains () =
  let calls = Atomic.make 0 in
  set_callback "count" (fun _ -> Atomic.incr calls; Py.none);
  let run = python_function "run_count" "
import threading
import threads_test

def run_count(n, k):
    def work():
        for _ in range(k):
            threads_test.count()
    threads = [threading.Thread(target=work) for _ in range(n)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
" in
  let totals = Py.Thread.run_ocaml (fun () ->
    let domains =
      List.init 4 (fun i -> Domain.spawn (fun () -> sum_with_python i)) in
    ignore (Py.Thread.call run
      (Py.Tuple.of_list [Py.Int.of_int 4; Py.Int.of_int 500]));
    List.map Domain.join domains) in
  if totals <> expected_sums 4 then Pyml_tests_common.Failed "wrong sums"
  else check "calls" (4 * 500) (Atomic.get calls)

(* The main thread holds the GIL while it allocates in [Py.Thread.run_ocaml]
   (which starts minor collections, stop-the-world sections that wait for
   all the domains whose lock is held), while domains wait for the GIL (in
   [Py.Thread.run_python], and when they leave a blocking section in
   [run_ocaml]), and Python threads that call OCaml wait for it too. They
   must wait for the GIL without holding the lock of their domain. With
   [~block:true], the domains first block, and wait for the GIL when they
   leave the blocking section (in the hook). *)
let collections_while_waiting_for_gil ~block () =
  let calls = Atomic.make 0 in
  set_callback "count" (fun _ ->
    Atomic.incr calls;
    ignore (Sys.opaque_identity (List.init 100 Fun.id));
    Py.none);
  let run = python_function "run_count" "
import threading
import threads_test

def run_count(n, k):
    def work():
        for _ in range(k):
            threads_test.count()
    threads = [threading.Thread(target=work) for _ in range(n)]
    for t in threads:
        t.start()
    return threads

def join(threads):
    for t in threads:
        t.join()
" in
  let join = Py.Module.get (Py.Module.main ()) "join" in
  let threads =
    Py.Thread.call run (Py.Tuple.of_list [Py.Int.of_int 2; Py.Int.of_int 200]) in
  let totals = Py.Thread.run_ocaml (fun () ->
    let domains = List.init 2 (fun i -> Domain.spawn (fun () ->
      if block then
        (* Blocks until the main thread allocates, then leaves the
           blocking section. *)
        Py.Thread.run_python (fun () ->
          Py.Thread.run_ocaml (fun () -> Unix.sleepf 0.2));
      for _ = 1 to 20 do
        Py.Thread.run_python (fun () ->
          Py.Thread.run_ocaml (fun () -> Unix.sleepf 0.001))
      done;
      sum_with_python i)) in
    (* Allocates for a while, holding the GIL (after letting the domains
       block). *)
    if block then Unix.sleepf 0.05;
    let t = Unix.gettimeofday () in
    while Unix.gettimeofday () -. t < 0.5 do
      ignore (Sys.opaque_identity (List.init 1000 Fun.id))
    done;
    List.map Domain.join domains) in
  ignore (Py.Thread.call join (Py.Tuple.of_list [threads]));
  if totals <> expected_sums 2 then Pyml_tests_common.Failed "wrong sums"
  else check "calls" (2 * 200) (Atomic.get calls)

(* [Py.Thread.call] releases the locks of OCaml, in [Py.Thread.run_ocaml]
   too: the Python code can wait for a Python thread that calls OCaml. *)
let python_waits_for_thread_calling_ocaml () =
  let calls = Atomic.make 0 in
  set_callback "incr" (fun _ -> Atomic.incr calls; Py.none);
  let run = python_function "run_and_wait" "
import threading
import threads_test

def run_and_wait():
    t = threading.Thread(target=threads_test.incr)
    t.start()
    t.join()
" in
  ignore (Py.Thread.call run Py.Tuple.empty);
  Py.Thread.run_ocaml (fun () -> ignore (Py.Thread.call run Py.Tuple.empty));
  check "calls" 2 (Atomic.get calls)

(* Python code called by OCaml code in [Py.Thread.run_ocaml] calls OCaml
   (which takes the world lock again), and [Py.Thread.run_python] is nested
   in [run_ocaml] and in itself. *)
let nested_calls () =
  let state = ref 0 in
  set_callback "add" (fun args ->
    state := !state + Py.Int.to_int args.(0);
    Py.none);
  let add = python_function "add_twice" "
import threads_test

def add_twice(n):
    threads_test.add(n)
    threads_test.add(n)
" in
  Py.Thread.run_ocaml (fun () ->
    ignore (Py.Callable.to_function add [| Py.Int.of_int 1 |]);
    ignore (Py.Thread.call add (Py.Tuple.of_list [Py.Int.of_int 10]));
    Py.Thread.run_python (fun () ->
      Py.Thread.run_python (fun () ->
        ignore (Py.Callable.to_function add [| Py.Int.of_int 100 |]))));
  check "state" 222 !state

(* Python code called by the comparison or the hash of Python objects in
   OCaml cannot call OCaml in [Py.Thread.run_ocaml] (the values compared are
   not roots, so that the domain lock cannot be released): it raises
   [RuntimeError]. With all the locks, it can. *)
let comparison_calls_ocaml () =
  set_callback "nothing" (fun _ -> Py.none);
  ignore (python_function "Calling" "
import threads_test

errors = []

class Calling:
    def call(self):
        try:
            threads_test.nothing()
        except RuntimeError as e:
            errors.append(str(e))
    def __eq__(self, other):
        self.call()
        return True
    def __hash__(self):
        self.call()
        return 0
");
  let calling = Py.Module.get (Py.Module.main ()) "Calling" in
  let errors () =
    Py.List.length (Py.Module.get (Py.Module.main ()) "errors") in
  let a = Py.Callable.to_function calling [| |] in
  let b = Py.Callable.to_function calling [| |] in
  assert (compare a b = 0);
  ignore (Hashtbl.hash a);
  if errors () <> 0 then Pyml_tests_common.Failed "error with all the locks"
  else begin
    Py.Thread.run_ocaml (fun () ->
      assert (compare a b = 0);
      ignore (Hashtbl.hash a));
    check "errors" 2 (errors ())
  end

(* The finalizers of the OCaml values that hold Python objects run in any
   thread: in a domain without the GIL, the Python objects are released
   later, by the next thread that takes the locks (otherwise, [__del__]
   runs without the GIL). *)
let release_without_gil () =
  let cls = python_function "Counted" "
deleted = [0]

class Counted:
    def __del__(self):
        deleted[0] += 1
" in
  let deleted () =
    Py.Int.to_int
      (Py.List.get_item (Py.Module.get (Py.Module.main ()) "deleted") 0) in
  let n = 100 in
  Py.Thread.run_ocaml (fun () ->
    ignore (spawn_domains 2 (fun _ ->
      let objects = Py.Thread.run_python (fun () ->
        List.init n (fun _ -> Py.Callable.to_function cls [| |])) in
      ignore (Sys.opaque_identity objects);
      (* The objects are unreachable: they are finalized here, without the
         GIL. *)
      Gc.full_major ())));
  Gc.full_major ();
  (* Takes the locks again. *)
  Py.Thread.run_ocaml ignore;
  check "deleted" (2 * n) (deleted ())

(* The Python API used by an OCaml domain without [Py.Thread.run_python]
   (without the GIL) raises [Failure], as the comparison of Python objects
   does. *)
let python_api_without_gil () =
  let a = Py.Int.of_int 1 and b = Py.Int.of_int 2 in
  let failures = Py.Thread.run_ocaml (fun () ->
    Domain.join (Domain.spawn (fun () ->
      let fails f = match f () with _ -> false | exception Failure _ -> true in
      [fails (fun () -> ignore (Py.Int.of_int 1));
       fails (fun () -> compare a b);
       fails (fun () -> Hashtbl.hash a)]))) in
  if List.for_all Fun.id failures then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed "no Failure"

(* The exceptions raised in [Py.Thread.run_ocaml] and [run_python] restore
   the locks: other threads and domains can call OCaml and Python again. *)
let exceptions_restore_locks () =
  set_callback "nothing" (fun _ -> Py.none);
  let run = python_function "run_thread" "
import threading
import threads_test

def run_thread():
    t = threading.Thread(target=threads_test.nothing)
    t.start()
    t.join()
" in
  (match Py.Thread.run_ocaml (fun () -> raise Exit) with
   | () -> assert false
   | exception Exit -> ());
  ignore (Py.Thread.call run Py.Tuple.empty);
  let totals = Py.Thread.run_ocaml (fun () ->
    spawn_domains 2 (fun i ->
      (match Py.Thread.run_python (fun () -> raise Exit) with
       | () -> assert false
       | exception Exit -> ());
      sum_with_python i)) in
  if totals = expected_sums 2 then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed "wrong sums"

(* Many Python threads call OCaml once: each one is registered in the OCaml
   runtime, and unregistered when it exits. (Without the unregistration,
   the runtime keeps a small descriptor of each thread: this test does not
   see the difference.) *)
let short_lived_threads () =
  let calls = Atomic.make 0 in
  set_callback "once" (fun _ -> Atomic.incr calls; Py.none);
  let run = python_function "run_many" "
import threading
import threads_test

def run_many(batches, n):
    for _ in range(batches):
        threads = [threading.Thread(target=threads_test.once)
                   for _ in range(n)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
" in
  ignore (Py.Thread.call run
    (Py.Tuple.of_list [Py.Int.of_int 50; Py.Int.of_int 20]));
  check "calls" 1000 (Atomic.get calls)

let scenarios = [
  "python_threads_call_ocaml", python_threads_call_ocaml;
  "domains_call_python", domains_call_python;
  "ocaml_threads_call_python", ocaml_threads_call_python;
  "world_lock", world_lock;
  "python_threads_and_domains", python_threads_and_domains;
  "collections_while_waiting_for_gil",
  collections_while_waiting_for_gil ~block:false;
  "collections_while_leaving_blocking_section",
  collections_while_waiting_for_gil ~block:true;
  "python_waits_for_thread_calling_ocaml",
  python_waits_for_thread_calling_ocaml;
  "nested_calls", nested_calls;
  "comparison_calls_ocaml", comparison_calls_ocaml;
  "release_without_gil", release_without_gil;
  "python_api_without_gil", python_api_without_gil;
  "exceptions_restore_locks", exceptions_restore_locks;
  "short_lived_threads", short_lived_threads;
]

(* Runs the scenario in a new process, killed after [timeout]. *)
let run_scenario name =
  Printf.printf "Scenario '%s' ...\n%!" name;
  let env =
    Array.append [| "PYML_THREADS_TEST=" ^ name |] (Unix.environment ()) in
  let pid =
    Unix.create_process_env Sys.executable_name Sys.argv env
      Unix.stdin Unix.stdout Unix.stderr in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait () =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then begin
          Unix.kill pid Sys.sigkill;
          ignore (Unix.waitpid [] pid);
          (* The line of the test is not terminated. *)
          Printf.printf "\nScenario '%s' timed out (deadlock?)\n%!" name;
          false
        end
        else begin
          Unix.sleepf 0.05;
          wait ()
        end
    | _, Unix.WEXITED 0 -> true
    | _, Unix.WEXITED n ->
        Printf.printf "Scenario '%s' failed (exit code %d)\n%!" name n;
        false
    | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) ->
        Printf.printf "\nScenario '%s' killed by signal %d\n%!" name n;
        false in
  wait ()

let () =
  match Sys.getenv_opt "PYML_THREADS_TEST" with
  | None ->
      let failed =
        List.filter (fun (name, _) -> not (run_scenario name)) scenarios in
      if failed <> [] then exit 1
  | Some name ->
      let scenario = List.assoc name scenarios in
      Pyml_tests_common.add_test ~title:name (fun () ->
        Py.Thread.enable ();
        ignore (Lazy.force python_sum : Py.Object.t array -> Py.Object.t);
        scenario ());
      Pyml_tests_common.main ()
