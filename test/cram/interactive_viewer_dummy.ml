open Cmdliner

let input_events : Notty.Unescape.event list ref = ref []
let output_image : Notty.image ref = ref Notty.I.empty

let open_dummy_terminal () : unit =
  let (input_fd, output_fd) = Unix.pipe () in
  let dev_null_fd = Unix.openfile "/dev/null" Unix.[O_WRONLY] 0o666 in
  Unix.dup2 input_fd Unix.stdin;
  Unix.dup2 dev_null_fd Unix.stdout;
  Unix.close input_fd;
  Unix.close output_fd

let print_image () : unit =
  let main_image = Notty.I.vcat [Notty.I.string Notty.A.empty "Hello World" ] in
  output_image := main_image;
  Notty_unix.output_image ~fd:stdout !output_image;
  print_endline "Hello world";
  flush stdout
  

let run_dummy_ui () : unit =
  let rec loop () =
    match !input_events with
    | [] -> ()
    | event :: events ->
      (* Process the event and update the output image *)
      match event with
      | `Key (`ASCII 'q', []) ->
        (* Exit the loop if the 'q' key is pressed *)
        ()
      | _ ->
        (* Ignore other event types *)
        ()
      ;

      input_events := events;
      print_image ();
      loop ()
  in
  print_image ();
  loop ()
let main () =
  open_dummy_terminal ();
  run_dummy_ui ()


let cmd =
  let doc = "Interactive Viewer Dummy" in
  let info = Cmd.info "interactive_viewer_dummy" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ const ())

let () =
  Stdlib.exit @@ (Cmd.eval cmd)

