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

let with_setup_term f =
  let is_tty = Unix.isatty Unix.stdin in
  if is_tty then
    let term =
      Notty_unix.Term.create ~input:Unix.stdin ~output:Unix.stdout ()
    in
    Fun.protect
      (fun () -> f is_tty term)
      ~finally:(fun () -> Notty_unix.Term.release term)
  else
    let tty_path = ttyname Unix.stdout in
    let tty_fd = Unix.openfile tty_path [ Unix.O_RDWR ] 0o600 in
    let term = Notty_unix.Term.create ~input:tty_fd ~output:tty_fd () in
    Fun.protect
      (fun () -> f is_tty term)
      ~finally:(fun () ->
        Notty_unix.Term.release term;
        Unix.close tty_fd)

let read_input ic =
  try In_channel.input_all ic with _ -> create_empty_diff_content ()

let main file_path =
  with_setup_term (fun is_tty term ->
      let input_content =
        match file_path with
        | Some path -> with_input_fd (Some path) read_input
        | None ->
            if is_tty then create_empty_diff_content ()
            else read_input In_channel.stdin
      in
      let patch = Patch.to_diffs input_content in
      InteractiveViewer.start ~term patch)

let file_arg =
  let doc = "Path to the input file." in
  Arg.(value & opt (some string) None & info [ "f"; "file" ] ~docv:"FILE" ~doc)

let cmd =
  let doc =
    "Render Git diffs in an accessible way. Acts as a pager if no file is \
     provided."
  in
  let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ file_arg)

let () = exit (Cmd.eval cmd)
