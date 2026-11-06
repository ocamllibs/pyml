(* Tests of the search of the Python library by [Py.initialize] with fake
   commands put first in the [PATH]. *)

let write_script directory name contents =
  let script = Filename.concat directory name in
  let channel = open_out script in
  output_string channel contents;
  close_out channel;
  Unix.chmod script 0o755;
  script

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec search i = i + m <= n && (String.sub s i m = sub || search (i + 1)) in
  search 0

(* When no library is found, the error gives the outcome of each heuristic
   and the libraries that failed to load. Only fake commands are in the
   [PATH]: an interpreter of a Python 3.99 that does not exist, and a
   [pkg-config] that gives a directory without library. *)
let test_not_found () =
  let directory = Filename.temp_dir "pyml_not_found" "" in
  let python =
    write_script directory "python3"
      "#!/bin/sh\nif [ \"$1\" = --version ]; then echo Python 3.99.0; fi\n" in
  let pkg_config =
    write_script directory "pkg-config" "#!/bin/sh\necho -L/nonexistent\n" in
  let path = Sys.getenv "PATH" in
  Unix.putenv "PATH" directory;
  begin match Py.initialize () with
  | () -> assert false
  | exception Failure msg ->
      List.iter (fun sub ->
        if not (contains msg sub) then
          failwith (Printf.sprintf "%S not in the error: %s" sub msg))
        ["Py.find_library: unable to find the Python library";
         "heuristics: interpreter: no candidate; ldconfig: no candidate; ";
         "pkg-config: libpython3.99m.";
         "/nonexistent/libpython3.99.";
         "PYTHONHOME: libpython3.99m.";
         "[/nonexistent/libpython3.99m."]
  end;
  Unix.putenv "PATH" path;
  Sys.remove python;
  Sys.remove pkg_config;
  Unix.rmdir directory

(* [Py.initialize] finds the Python library even if [pkg-config] gives no
   [-l] flag for it, as with the framework builds of Python on macOS
   (Homebrew): [pkg-config --libs python-3.13] gives only
   [-L/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/lib]. *)
let test_pkg_config_without_l () =
  let directory = Filename.temp_dir "pyml_pkg_config" "" in
  let script =
    write_script directory "pkg-config"
      "#!/bin/sh\n\
       echo \"-L/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/\
       Versions/3.13/lib\"\n" in
  Unix.putenv "PATH" (directory ^ ":" ^ Sys.getenv "PATH");
  Py.initialize ();
  assert (Py.Int.to_int (Py.Run.eval "1 + 1") = 2);
  Sys.remove script;
  Unix.rmdir directory

let () =
  test_not_found ();
  test_pkg_config_without_l ()
