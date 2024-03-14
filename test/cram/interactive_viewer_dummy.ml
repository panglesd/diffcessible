open Cmdliner

let input_events : Notty.Unescape.event list ref = ref []
let output_image : Notty.image ref = ref Notty.I.empty

let print_image () : unit =
  let main_image = Notty.I.vcat [Notty.I.string Notty.A.empty "Hello World" ] in
  output_image := main_image;
  Notty_unix.output_image ~fd:stdout !output_image;

  print_endline ""


let run_dummy_ui () : unit =
  let rec loop () =
    let ready_fds, _, _ = Unix.select [Unix.stdin] [] [] (-1.0) in
    if List.mem Unix.stdin ready_fds then (
      try
        let input_char = input_char stdin in  (* Read a single character from stdin *)
        let input_event = `Key (`ASCII input_char, []) in
        match input_event with
        | `Key (`ASCII 'q', []) -> ()
        | _ ->
          ()
        ;
        print_image ();
        loop ()
      with
      |End_of_file -> ()
    ) else (
      loop ()
    )
  in
  print_image ();
  loop ()
let main (_input_events : char list) =
  input_events := List.map (fun c -> `Key (`ASCII c, [])) _input_events;
  run_dummy_ui ()

let input_events_term =
  let doc = "Input event as a single character" in
 Arg.(value & pos_all char [] & info [] ~docv:"EVENT" ~doc)

let cmd =
  let doc = "Interactive Viewer Dummy" in
  let info = Cmd.info "interactive_viewer_dummy" ~version:"VERSION" ~doc in
  Cmd.v info Term.(const main $  input_events_term)
  
let () = exit @@ Cmd.eval cmd
