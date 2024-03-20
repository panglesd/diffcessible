open Diffcessible
open Cmdliner

let main file_path =
  let file = In_channel.open_bin file_path in
  let s = In_channel.input_all file in
  let patch = Patch.to_diffs s in
  Interactive_viewer.start patch [] false

let file_arg =
  let doc = "Path to the file containing the Git diff." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Render Git diffs in an accessible way." in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ file_arg)

let () = exit (Cmd.eval cmd)
