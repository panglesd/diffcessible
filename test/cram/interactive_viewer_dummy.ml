open Cmdliner

let input_events : Notty.Unescape.event list ref = ref []
let output_image : Notty.image ref = ref Notty.I.empty

let open_dummy_terminal () : unit =
  let input_fd = Unix.openfile "/dev/null" Unix.[O_RDWR] 0o666 in
  let output_fd = Unix.dup Unix.stdout in
  Unix.dup2 input_fd Unix.stdin;
  Unix.dup2 output_fd Unix.stdout;
  Unix.close input_fd;
  Unix.close output_fd


let output_image : Notty.image = !output_image

let print_image () : unit =
  Notty_unix.output_image output_image;
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

