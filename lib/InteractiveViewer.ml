open Nottui
module W = Nottui_widgets
open Lwd_infix

type view_mode = SideBySide | Normal

let view_mode : view_mode Lwd.var = Lwd.var Normal

let toggle_view_mode () : unit =
  match Lwd.peek view_mode with
  | Normal -> Lwd.set view_mode SideBySide
  | SideBySide -> Lwd.set view_mode Normal

let help_visible = Lwd.var false
let quit = Lwd.var false

let toggle_help_visibility () =
  Lwd.set help_visible (not (Lwd.peek help_visible))

let view (patches : Patch.t list) =
  let z_patches_var : Patch.t Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let hunks_ui =
    let$ mode = Lwd.get view_mode and$ z_patches = Lwd.get z_patches_var in
    match mode with
    | Normal -> HunkView.current_hunks z_patches
    | SideBySide -> HunkView.current_hunks_side_by_side z_patches
  in
  let curr_scroll_state = Lwd.var W.default_scroll_state in
  let change_scroll_state _action state =
    let off_screen = state.W.position > state.W.bound in
    if off_screen then
      Lwd.set curr_scroll_state { state with position = state.W.bound }
    else Lwd.set curr_scroll_state state
  in
  let ui =
    let$ help_visible = Lwd.get help_visible in
    if help_visible then
      W.vbox
        [
          W.scrollbox @@ Lwd.pure @@ HelpView.help_panel;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     toggle_help_visibility ();
                     `Handled
                 | _ -> `Unhandled)
               (W.string "Type 'q' to exit the help panel");
        ]
    else
      let z_patches = Lwd.get z_patches_var in
      W.vbox
        [
          Lwd.map z_patches ~f:OperationView.operation_info;
          Lwd.map z_patches ~f:OperationView.change_summary;
          Lwd.map z_patches ~f:OperationView.current_operation;
          W.vscroll_area
            ~state:(Lwd.get curr_scroll_state)
            ~change:change_scroll_state hunks_ui;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set quit true;
                     `Handled
                 | `ASCII 'n', [] ->
                     Lwd.set z_patches_var
                       (PatchNavigation.navigate PatchNavigation.Next
                          (Lwd.peek z_patches_var));
                     `Handled
                 | `ASCII 'p', [] ->
                     Lwd.set z_patches_var
                       (PatchNavigation.navigate PatchNavigation.Prev
                          (Lwd.peek z_patches_var));
                     `Handled
                 | `ASCII 'h', [] ->
                     toggle_help_visibility ();
                     `Handled
                 | `ASCII 't', [] ->
                     toggle_view_mode ();
                     `Handled
                 | _ -> `Unhandled)
               (W.string
                  "Type 'h' to go to the help panel, 'q' to quit, 'n' to go to \
                   the next operation, 'p' to go to the previous operation. \
                   Press 't' to toggle view mode.");
        ]
  in
  Lwd.return ui

let start ?term patch =
  Ui_loop.run ?term ~quit ~tick_period:0.2 (Lwd.bind ~f:Lwd.join (view patch))

(* Tests *)
let start_test patch events width height =
  let convert_char_to_key (c : char) : Ui.key = (`ASCII c, []) in
  let content_ui_root = Lwd.observe (Lwd.bind ~f:Lwd.join (view patch)) in
  let ui_renderer =
    let renderer = Renderer.make () in
    Renderer.update renderer (width, height) (Lwd.quick_sample content_ui_root);
    renderer
  in
  let rec process_events (events : char list) =
    match events with
    | [] -> ()
    | event :: rest ->
        let ui_event = convert_char_to_key event in
        ignore (Renderer.dispatch_key ui_renderer ui_event);
        Renderer.update ui_renderer (width, height)
          (Lwd.quick_sample content_ui_root);
        process_events rest
  in
  process_events events;
  let init_image = Renderer.image ui_renderer in
  Notty_unix.output_image init_image;
  Lwd.quick_release content_ui_root;
  print_newline ()
