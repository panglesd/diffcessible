open Cmdliner

let input_events : Notty.Unescape.event list ref = ref []
let output_image : Notty.image ref = ref Notty.I.empty

let print_image () : unit =
  let main_image =
    Notty.I.vcat [ Notty.I.string Notty.A.empty "Hello World" ]
  in
  output_image := main_image;
  Notty_unix.output_image ~fd:stdout !output_image;

  print_endline ""

let run_dummy_ui () : unit =
  let rec loop events =
    match events with
    | [] -> ()
    | event :: rest -> (
        match event with `Key (`ASCII 'q', []) -> () | _ -> loop rest)
  in
  print_image ();
  loop !input_events

let main (_input_events : char list) =
  input_events := List.map (fun c -> `Key (`ASCII c, [])) _input_events;
  run_dummy_ui ()

let input_events_term =
  let doc = "Input event as a single character" in
  Arg.(value & pos_all char [] & info [] ~docv:"EVENT" ~doc)

let cmd =
  let doc = "Interactive Viewer Dummy" in
  let info = Cmd.info "interactive_viewer_dummy" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $ input_events_term)

let () = exit @@ Cmd.eval cmd
