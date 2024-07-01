open Diffcessible
open Cmdliner
open ExtUnix.Specific

let main file_path =
  let file = match file_path with
    | Some path -> In_channel.open_bin path
    | None ->
        let tty = ttyname Unix.stdin 
  in In_channel.open_bin tty
  in let s = In_channel.input_all file
  in let patch = Patch.to_diffs s in
  Interactive_viewer.start patch

let file_arg =
  let doc = "Path to the file containing the Git diff. If not provided, reads from the terminal." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc) 

let cmd =
  let doc = "Render Git diffs in an accessible way. Acts as a pager if no file is provided." in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ file_arg)

let () = exit (Cmd.eval cmd)

