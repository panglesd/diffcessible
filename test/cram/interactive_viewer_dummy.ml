open Cmdliner
open Diffcessible

let main file_path custom_inputs =
  let file = In_channel.open_bin file_path in
  let s = In_channel.input_all file in
  let patch = Patch.to_diffs s in
  Interactive_viewer.start_test patch custom_inputs

let file_arg =
  let doc = "Path to the file containing the Git diff." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let custom_inputs_arg =
  let doc = "Custom inputs." in
  Arg.(value & pos_right 0 char [] & info [] ~docv:"INPUTS" ~doc)

let width_arg =
  let doc = "Width of the content image." in
  Arg.(value & opt int 150 & info [ "width" ] ~docv:"WIDTH" ~doc)

let height_arg =
  let doc = "Height of the content image." in
  Arg.(value & opt int 20 & info [ "height" ] ~docv:"HEIGHT" ~doc)

let cmd =
  let doc = "Render Git diffs in an accessible way." in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info
    Term.(const main $ file_arg $ custom_inputs_arg $ width_arg $ height_arg)

let () = exit @@ Cmd.eval cmd
