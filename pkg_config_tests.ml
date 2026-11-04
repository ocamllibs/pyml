(* [Py.initialize] finds the Python library even if [pkg-config] gives no
   [-l] flag for it, as with the framework builds of Python on macOS
   (Homebrew): [pkg-config --libs python-3.13] gives only
   [-L/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13/lib].
   A fake [pkg-config] with such an output is put first in the [PATH]. *)

let () =
  let directory = Filename.temp_dir "pyml_pkg_config" "" in
  let script = Filename.concat directory "pkg-config" in
  let channel = open_out script in
  output_string channel
    "#!/bin/sh\n\
     echo \"-L/opt/homebrew/opt/python@3.13/Frameworks/Python.framework/\
     Versions/3.13/lib\"\n";
  close_out channel;
  Unix.chmod script 0o755;
  Unix.putenv "PATH" (directory ^ ":" ^ Sys.getenv "PATH");
  Py.initialize ();
  assert (Py.Int.to_int (Py.Run.eval "1 + 1") = 2);
  Sys.remove script;
  Unix.rmdir directory
