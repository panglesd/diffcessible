open Diffcessible

let main () =
  Interactive_viewer.start ()

open Cmdliner

let cmd =
  let doc = "Render Git diffs in an accessible way." in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ const ())

let () = exit (Cmd.eval cmd)
