(* Tests of [Py.Thread]: Python threads that call OCaml, and OCaml domains
   that call Python. Built with OCaml 5 only (see dune). *)

let python_threads_call_ocaml () =
  let calls = Atomic.make 0 in
  let callback _args =
    Atomic.incr calls;
    (* Allocates in OCaml and in Python. *)
    Py.Int.of_int (List.length (List.init 100 Fun.id)) in
  let m = Py.Import.add_module "threads_test" in
  Py.Module.set m "callback" (Py.Callable.of_function callback);
  assert (Py.Run.simple_string "
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
");
  let run = Py.Module.get (Py.Module.main ()) "run" in
  (* The main thread holds the locks of OCaml: [Py.Thread.call] releases
     them while the Python threads call [callback]. *)
  ignore (Py.Thread.call run (Py.Tuple.of_list [Py.Int.of_int 8; Py.Int.of_int 500]));
  if Atomic.get calls = 8 * 500 then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed (Printf.sprintf "%d calls" (Atomic.get calls))

let domains_call_python () =
  let sum = Py.Module.get_function (Py.Import.import_module "builtins") "sum" in
  let work i () =
    let total = ref 0 in
    for j = 1 to 200 do
      total := !total + Py.Thread.run_python (fun () ->
        let l = Py.List.of_list_map Py.Int.of_int [i; j] in
        Py.Int.to_int (sum [| l |]))
    done;
    !total in
  (* [Domain.join] blocks: [run_ocaml] lets the domains take the locks of
     Python meanwhile. *)
  let totals = Py.Thread.run_ocaml (fun () ->
    List.init 4 (fun i -> Domain.spawn (work i)) |> List.map Domain.join) in
  let expected = List.init 4 (fun i -> 200 * i + 200 * 201 / 2) in
  if totals = expected then Pyml_tests_common.Passed
  else Pyml_tests_common.Failed "wrong sums"

let () =
  Pyml_tests_common.add_test ~title:"enable threads" (fun () ->
    Py.Thread.enable ();
    assert (Py.Thread.enabled ());
    Pyml_tests_common.Passed);
  Pyml_tests_common.add_test ~title:"Python threads call OCaml"
    python_threads_call_ocaml;
  Pyml_tests_common.add_test ~title:"OCaml domains call Python"
    domains_call_python

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
