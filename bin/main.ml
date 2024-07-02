open Diffcessible
open Cmdliner
open ExtUnix.Specific

let main file_path =
  let input_channel, term =
    match file_path with
    | Some path -> (In_channel.open_bin path, None)
    | None ->
        let tty_path = ttyname Unix.stdout in
        let tty_fd = Unix.openfile tty_path [ Unix.O_RDWR ] 0o600 in
        let term = Notty_unix.Term.create ~output:tty_fd ~input:tty_fd () in
        (In_channel.stdin, Some term)
  in
  let input_content = In_channel.input_all input_channel in
  In_channel.close input_channel;
  let patch = Patch.to_diffs input_content in
  Interactive_viewer.start ?term patch;
  match term with Some t -> Notty_unix.Term.release t | None -> ()

let file_arg =
  let doc =
    "Path to the file containing the Git diff. If not provided, reads from \
     stdin."
  in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc =
    "Render Git diffs in an accessible way. Acts as a pager if no file is \
     provided."
  in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ file_arg)

let () = exit (Cmd.eval cmd)
