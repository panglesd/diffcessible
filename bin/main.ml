open Diffcessible

let main () =
  let file = In_channel.open_bin "diff.example" in
  let s = In_channel.input_all file in
  let patch = Patch.to_diffs s in
  Interactive_viewer.start patch

open Cmdliner

let cmd =
  let doc = "Render Git diffs in an accessible way." in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ const ())

let () = exit (Cmd.eval cmd)
