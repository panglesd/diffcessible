open Nottui
module W = Nottui_widgets
open Lwd_infix

let operation_info z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let num_hunks = List.length p.Patch.hunks in
  let hunk_text =
    match num_hunks with
    | 1 -> "1 hunk"
    | _ -> Printf.sprintf "%d hunks" num_hunks
  in
  W.string
    ~attr:Notty.A.(fg lightcyan)
    (Printf.sprintf "Operation %d of %d, %s"
       (Zipper.get_current_index z + 1)
       (Zipper.get_total_length z)
       hunk_text)

let ui_of_operation operation =
  let green_bold_attr = Notty.A.(fg green ++ st bold) in
  let red_bold_attr = Notty.A.(fg red ++ st bold) in
  let blue_bold_attr = Notty.A.(fg blue ++ st bold) in
  match operation with
  | Patch.Create path ->
      Ui.hcat [ W.string "Creation of "; W.string ~attr:green_bold_attr path ]
  | Patch.Delete path ->
      Ui.hcat [ W.string "Deletion of "; W.string ~attr:red_bold_attr path ]
  | Patch.Rename (old_path, new_path) ->
      Ui.hcat
        [
          W.string "Rename with modifications ";
          W.string ~attr:blue_bold_attr old_path;
          W.string " to ";
          W.string ~attr:green_bold_attr new_path;
        ]
  | Patch.Rename_only (old_path, new_path) ->
      Ui.hcat
        [
          W.string "Rename ";
          W.string ~attr:blue_bold_attr old_path;
          W.string " to ";
          W.string ~attr:green_bold_attr new_path;
        ]
  | Patch.Edit path ->
      Ui.hcat
        [ W.string "Modification of "; W.string ~attr:blue_bold_attr path ]

let string_of_hunk = Format.asprintf "%a" Patch.pp_hunk

let current_operation z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  ui_of_operation p.Patch.operation

let current_hunks z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks = List.map (fun h -> W.string (string_of_hunk h)) p.Patch.hunks in
  Ui.vcat @@ hunks

type direction = Prev | Next

let navigate z_patches (dir : direction) : unit =
  let z = Lwd.peek z_patches in
  match dir with
  | Prev -> Lwd.set z_patches (Zipper.prev z)
  | Next -> Lwd.set z_patches (Zipper.next z)

let quit = Lwd.var false
let help = Lwd.var false

let additions_and_removals lines =
  let add_line (additions, removals) line =
    match line with
    | `Their _ -> (additions + 1, removals)
    | `Mine _ -> (additions, removals + 1)
    | `Common _ -> (additions, removals)
  in
  List.fold_left add_line (0, 0) lines

let accumulate_count hunks =
  List.fold_left
    (fun (add_acc, remove_acc) hunk ->
      let add_in_hunk, remove_in_hunk =
        additions_and_removals hunk.Patch.lines
      in
      (add_acc + add_in_hunk, remove_acc + remove_in_hunk))
    (0, 0) hunks

let change_summary z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let total_additions, total_removals = accumulate_count p.Patch.hunks in
  let format_plural n singular plural =
    if n = 1 then Printf.sprintf "%d %s" n singular
    else Printf.sprintf "%d %s" n plural
  in
  let operation_count =
    Printf.sprintf "%s, %s"
      (format_plural total_additions "addition" "additions")
      (format_plural total_removals "removal" "removals")
  in
  W.string ~attr:Notty.A.(fg lightcyan) operation_count

let view (patches : Patch.t list) =
  let help_panel =
    Ui.vcat
      [
        W.string "Help Panel:\n";
        W.string "h:   Open the help panel";
        W.string "q:   Quit the diffcessible viewer";
        W.string "n:   Move to the next operation, if present";
        W.string "p:   Move to the previous operation, if present";
        W.string "g:   Scroll back to the top of the displayed operation.";
      ]
  in
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let curr_scroll_state = Lwd.var W.default_scroll_state in
  let curr_scroll_state_h = Lwd.var W.default_scroll_state in
  let change_scroll_state _action state =
    let off_screen = state.W.position > state.W.bound in
    if off_screen then
      Lwd.set curr_scroll_state { state with position = state.W.bound }
    else Lwd.set curr_scroll_state state
  in
  let change_scroll_state_h _action state =
    let off_screen = state.W.position > state.W.bound in
    if off_screen then
      Lwd.set curr_scroll_state_h { state with position = state.W.bound }
    else Lwd.set curr_scroll_state_h state
  in
  let ui =
    let$* help_visible = Lwd.get help in
    if help_visible then
      W.vbox
        [
          W.scrollbox @@ Lwd.pure @@ help_panel;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set help false;
                     `Handled
                 | _ -> `Unhandled)
               (W.string "Type 'q' to exit the help panel");
        ]
    else
      W.vbox
        [
          operation_info z_patches;
          change_summary z_patches;
          current_operation z_patches;
          (* W.vscroll_area
               ~state:(Lwd.get curr_scroll_state)
               ~change:change_scroll_state
             @@ W.hscroll_area
                  ~state:(Lwd.get curr_scroll_state_h)
                  ~change:change_scroll_state_h
             @@ current_hunks z_patches; *)
          W.hscroll_area
            ~state:(Lwd.get curr_scroll_state_h)
            ~change:change_scroll_state_h
          @@ W.vscroll_area
               ~state:(Lwd.get curr_scroll_state)
               ~change:change_scroll_state
          @@ current_hunks z_patches;
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     Lwd.set quit true;
                     `Handled
                 | `ASCII 'n', [] ->
                     navigate z_patches Next;
                     `Handled
                 | `ASCII 'p', [] ->
                     navigate z_patches Prev;
                     `Handled
                 | `ASCII 'h', [] ->
                     Lwd.set help true;
                     `Handled
                 | `ASCII 'g', [] ->
                     Lwd.set curr_scroll_state
                       { (Lwd.peek curr_scroll_state) with W.position = 0 };
                     `Handled
                 | _ -> `Unhandled)
               (W.string
                  "Type 'h' to go to the help panel, 'q' to quit, 'n' to go to \
                   the next operation, 'p' to go to the previous operation");
        ]
  in
  W.vbox [ ui ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)

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
