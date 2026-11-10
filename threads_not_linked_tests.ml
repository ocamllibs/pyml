(* [Py.Thread.enable] fails if the library threads is not linked (this test
   is not linked with it). *)

let () =
  Pyml_tests_common.add_test ~title:"enable without threads" (fun () ->
    match Py.Thread.enable () with
    | () -> Pyml_tests_common.Failed "no failure"
    | exception Failure msg when Py.Thread.enabled () = false ->
        if String.length msg > 0 then Pyml_tests_common.Passed
        else Pyml_tests_common.Failed "empty message")

let () =
  if not !Sys.interactive then
    Pyml_tests_common.main ()
