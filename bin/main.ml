open Diffcessible
open Cmdliner
open ExtUnix.Specific

let create_empty_diff_content () =
  "diff --git a/Error b/Error\n--- a/Error\n+++ b/Error\n"

let setup_input = function
  | Some path ->
      if not (Sys.file_exists path) then failwith ("File not found: " ^ path);
      let ic = In_channel.open_bin path in
      (Some ic, Notty_unix.Term.create ())
  | None ->
      let tty_path = ttyname Unix.stdout in
      let tty_fd = Unix.openfile tty_path [ Unix.O_RDWR ] 0o600 in
      let term = Notty_unix.Term.create ~input:tty_fd ~output:tty_fd () in
      if not (Unix.isatty Unix.stdin) then (Some In_channel.stdin, term)
      else (None, term)

let safe_read_input ic =
  try
    let content = In_channel.input_all ic in
    In_channel.close ic;
    content
  with _ ->
    In_channel.close ic;
    create_empty_diff_content ()

let main file_path =
  let ic_opt, term = setup_input file_path in
  let input_content =
    match ic_opt with
    | None -> create_empty_diff_content ()
    | Some ic -> safe_read_input ic
  in
  let patch = Patch.to_diffs input_content in
  Interactive_viewer.start ~term patch;
  Notty_unix.Term.release term;
  (* restore terminal to original state *)
  Unix.tcsetattr Unix.stdin Unix.TCSANOW (Unix.tcgetattr Unix.stdin)

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
