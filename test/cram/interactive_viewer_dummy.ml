open Cmdliner

let input_events : Notty.Unescape.event list ref = ref []
let output_image : Notty.image ref = ref Notty.I.empty

let print_image () : unit =
  let main_image = Notty.I.vcat [Notty.I.string Notty.A.empty "Hello World" ] in
  output_image := main_image;
  Notty_unix.output_image ~fd:stdout !output_image;
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
  run_dummy_ui () 

let input_events_term =
  let doc = "Input event as a single character" in
  Cmdliner.Arg.(required & opt (some char) None & info ["input-event"] ~docv:"EVENT" ~doc)

let cmd =
  let doc = "Interactive Viewer Dummy" in
  let info = Cmd.info "interactive_viewer_dummy" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ const ())
  
  let () =
  Stdlib.exit @@ (Cmd.eval cmd)

