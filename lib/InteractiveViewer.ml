open Nottui
module W = Nottui_widgets
open Lwd_infix

let view (patches : Patch.t list) =
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let hunks_ui =
    Lwd.bind (Lwd.get HunkView.view_mode) ~f:(fun mode ->
        match mode with
        | Normal -> HunkView.current_hunks z_patches
        | SideBySide -> HunkView.current_hunks_side_by_side z_patches)
  in
  let curr_scroll_state = Lwd.var W.default_scroll_state in
  let change_scroll_state _action state =
    let off_screen = state.W.position > state.W.bound in
    if off_screen then
      Lwd.set curr_scroll_state { state with position = state.W.bound }
    else Lwd.set curr_scroll_state state
  in
  let ui =
    let$* help_visible = Lwd.get HelpView.help_visible in
    if help_visible then
      W.vbox
        [
          W.scrollbox @@ Lwd.pure @@ HelpView.help_panel;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     HelpView.toggle_help_visibility ();
                     `Handled
                 | _ -> `Unhandled)
               (W.string "Type 'q' to exit the help panel");
        ]
    else
      W.vbox
        [
          OperationView.operation_info z_patches;
          PatchNavigation.change_summary z_patches;
          OperationView.current_operation z_patches;
          W.vscroll_area
            ~state:(Lwd.get curr_scroll_state)
            ~change:change_scroll_state hunks_ui;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set PatchNavigation.quit true;
                     `Handled
                 | `ASCII 'n', [] ->
                     PatchNavigation.navigate z_patches PatchNavigation.Next;
                     `Handled
                 | `ASCII 'p', [] ->
                     PatchNavigation.navigate z_patches PatchNavigation.Prev;
                     `Handled
                 | `ASCII 'h', [] ->
                     HelpView.toggle_help_visibility ();
                     `Handled
                 | `ASCII 't', [] ->
                     HunkView.toggle_view_mode ();
                     `Handled
                 | _ -> `Unhandled)
               (W.string
                  "Type 'h' to go to the help panel, 'q' to quit, 'n' to go to \
                   the next operation, 'p' to go to the previous operation. \
                   Press 't' to toggle view mode.");
        ]
  in
  W.vbox [ ui ]

let start patch =
  Ui_loop.run ~quit:PatchNavigation.quit ~tick_period:0.2 (view patch)

(* Tests *)

let start_test patch events width height =
  let convert_char_to_key (c : char) : Ui.key = (`ASCII c, []) in
  let content_ui_root = Lwd.observe (view patch) in
  let content_ui = Lwd.quick_sample content_ui_root in
  let ui_renderer =
    let renderer = Renderer.make () in
    Renderer.update renderer (width, height) content_ui;
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
