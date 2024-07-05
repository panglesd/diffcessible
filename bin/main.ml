open Diffcessible
open Cmdliner
open ExtUnix.Specific

let create_empty_diff_content () =
  "diff --git a/Error b/Error\n--- a/Error\n+++ b/Error\n"

let with_input_fd file_path f =
  match file_path with
  | Some path ->
      if not (Sys.file_exists path) then failwith ("File not found: " ^ path);
      In_channel.with_open_bin path f
  | None -> f In_channel.stdin

let setup_term () =
  let tty_path = ttyname Unix.stdout in
  let tty_fd = Unix.openfile tty_path [ Unix.O_RDWR ] 0o600 in
  let term = Notty_unix.Term.create ~input:tty_fd ~output:tty_fd () in
  (Unix.isatty Unix.stdin, term, tty_fd)

let read_input ic =
  try In_channel.input_all ic with _ -> create_empty_diff_content ()

let main file_path =
  let is_tty, term, tty_fd = setup_term () in
  Fun.protect
    (fun () ->
      let input_content =
        if is_tty then
          match file_path with
          | Some _ -> with_input_fd file_path read_input
          | None -> create_empty_diff_content ()
        else
          Fun.protect
            (fun () -> read_input In_channel.stdin)
            ~finally:(fun () -> In_channel.close In_channel.stdin)
      in
      let patch = Patch.to_diffs input_content in
      InteractiveViewer.start ~term patch)
    ~finally:(fun () ->
      Notty_unix.Term.release term;
      Unix.close tty_fd)

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
