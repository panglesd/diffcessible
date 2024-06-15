open Diffcessible
open Async

let read_content file_path_opt =
  match file_path_opt with
  | Some path ->
      let file = In_channel.open_bin path in
      let content = In_channel.input_all file in
      In_channel.close file;
      return content
  | None -> Reader.contents (Lazy.force Reader.stdin)

let process_diffs content =
  let patch = Patch.to_diffs content in
  Interactive_viewer.start patch

let main file_path_opt () =
  let open Deferred.Let_syntax in
  let%bind content = read_content file_path_opt in
  process_diffs content;
  return ()

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:
      "Start the diffcessible viewer. Reads from a file or stdin if no file \
       path is provided."
    (let%map_open file_path =
       flag "-file" (optional string)
         ~doc:
           "FILE Path to the file containing the Git diff. Reads from stdin if \
            not provided."
     in
     main file_path)

let () =
  Command_unix.run
    (Command.group ~summary:"Render Git diffs in an accessible way."
       [ ("diffcessible", command) ])

(* let main () = *)
(*   let file = In_channel.open_bin "diff.example" in *)
(*   let s = In_channel.input_all file in *)
(*   let patch = Patch.to_diffs s in *)
(*   Interactive_viewer.start patch *)
(**)
(* open Cmdliner *)
(**)
(* let cmd = *)
(*   let doc = "Render Git diffs in an accessible way." in *)
(*   let info = Cmd.info "diffcessible" ~version:"VERSION" ~doc in *)
(*   Cmd.v info Term.(const main $ const ()) *)
(**)
(* let () = exit (Cmd.eval cmd) *)
