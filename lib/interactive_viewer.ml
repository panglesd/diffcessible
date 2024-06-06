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

let line_numbers_visible = Lwd.var false

let toggle_line_numbers () =
  (* Need to toggle refresh when used so lines become visible *)
  let current = Lwd.peek line_numbers_visible in
  Lwd.set line_numbers_visible (not current)

let ui_of_hunk hunk =
  let line_to_string i line =
    let line_number = Printf.sprintf "%4d " (i + 1) in
    match line with
    | `Their text -> W.string ~attr:Notty.A.(fg red) (line_number ^ text)
    | `Mine text -> W.string ~attr:Notty.A.(fg green) (line_number ^ text)
    | `Common text -> W.string (line_number ^ text)
  in

  Ui.vcat @@ List.mapi line_to_string hunk.Patch.lines

let current_operation z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  ui_of_operation p.Patch.operation

let current_hunks z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks = List.map ui_of_hunk p.Patch.hunks in
  Ui.vcat hunks

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

(** Side by side diff view implementation **)

type view_mode = SideBySide | Normal

let view_mode = Lwd.var Normal

let toggle_view_mode () =
  match Lwd.peek view_mode with
  | Normal -> Lwd.set view_mode SideBySide
  | SideBySide -> Lwd.set view_mode Normal

let rec split_and_align_hunk hunks mine_acc their_acc =
  match hunks with
  | [] -> (List.rev mine_acc, List.rev their_acc)
  | (`Common _ as common) :: t ->
      split_and_align_hunk t (common :: mine_acc) (common :: their_acc)
  | `Mine s :: `Their s' :: t ->
      split_and_align_hunk t (`Mine s :: mine_acc) (`Their s' :: their_acc)
  | `Mine s :: t ->
      split_and_align_hunk t (`Mine s :: mine_acc) (`Common "" :: their_acc)
  | `Their s :: t ->
      split_and_align_hunk t (`Common "" :: mine_acc) (`Their s :: their_acc)

let wrap_text s max_width =
  let rec aux acc current =
    if String.length current > max_width then
      let space_index =
        try Some (String.rindex_from current (max_width - 1) ' ')
        with Not_found -> None
      in
      match space_index with
      | Some idx ->
          let line = String.sub current 0 idx in
          let rest =
            String.sub current (idx + 1) (String.length current - idx - 1)
          in
          aux (line :: acc) rest
      | None ->
          let line = String.sub current 0 max_width in
          let rest =
            String.sub current max_width (String.length current - max_width)
          in
          aux (line :: acc) rest
    else List.rev (current :: acc)
  in
  aux [] s

let max_segment_width hunks =
  let rec aux max_width lines =
    match lines with
    | [] -> max_width
    | (`Common s | `Mine s | `Their s) :: rest ->
        let width = String.length s in
        aux (max max_width width) rest
  in
  List.fold_left
    (fun acc hunk ->
      let mine_lines, their_lines =
        split_and_align_hunk hunk.Patch.lines [] []
      in
      let lines = List.concat [ mine_lines; their_lines ] in
      aux acc lines)
    0 hunks

let lines_to_ui_with_numbers lines max_width attr_line_number attr_change =
  let line_num = ref 0 in
  List.flatten
    (List.map
       (fun line ->
         let content, attr =
           match line with
           | `Common s -> (s, attr_line_number)
           | `Mine s -> (s, attr_change)
           | `Their s -> (s, attr_change)
         in
         let wrapped_lines = wrap_text content max_width in
         List.mapi
           (fun i wrapped_line ->
             if wrapped_line <> "" then (
               if i = 0 then incr line_num;
               (* Only increment on the first segment of non-empty wrapped line *)
               let line_label =
                 if i = 0 then Printf.sprintf "%4d " !line_num else "     "
               in
               Ui.hcat
                 [
                   W.string ~attr:attr_line_number line_label;
                   W.string ~attr wrapped_line;
                 ])
             else
               Ui.hcat [ W.string ~attr "     "; W.string ~attr wrapped_line ])
           wrapped_lines)
       lines)

let ui_of_hunk_side_by_side hunk max_width =
  let mine_lines, their_lines = split_and_align_hunk hunk.Patch.lines [] [] in

  let attr_line_number = Notty.A.(fg lightblue) in
  let attr_mine = Notty.A.(fg red ++ st bold) in
  let attr_their = Notty.A.(fg green ++ st bold) in

  let mine_ui =
    lines_to_ui_with_numbers mine_lines max_width attr_line_number attr_mine
  in
  let their_ui =
    lines_to_ui_with_numbers their_lines max_width attr_line_number attr_their
  in
  let space = Ui.space 1 0 in
  Ui.hcat
    [
      Ui.resize ~sw:1 (Ui.vcat mine_ui);
      space;
      Ui.resize ~sw:1 (Ui.vcat their_ui);
    ]

let current_hunks_side_by_side z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let max_width = max_segment_width p.Patch.hunks in
  let hunks_ui =
    List.map (fun h -> ui_of_hunk_side_by_side h max_width) p.Patch.hunks
  in
  Ui.vcat @@ hunks_ui

(** end of side by side diff view implementation **)

let view (patches : Patch.t list) =
  let help_panel =
    Ui.vcat
      [
        W.string "Help Panel:\n";
        W.string "h:   Open the help panel";
        W.string "q:   Quit the diffcessible viewer";
        W.string "n:   Move to the next operation, if present";
        W.string "p:   Move to the previous operation, if present";
        W.string "t:   Toggle view mode";
        W.string "l:   Toggle line numbers";
      ]
  in
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let hunks_ui =
    Lwd.bind (Lwd.get view_mode) ~f:(fun mode ->
        match mode with
        | Normal -> current_hunks z_patches
        | SideBySide -> current_hunks_side_by_side z_patches)
  in
  let curr_scroll_state = Lwd.var W.default_scroll_state in
  let change_scroll_state _action state =
    let off_screen = state.W.position > state.W.bound in
    if off_screen then
      Lwd.set curr_scroll_state { state with position = state.W.bound }
    else Lwd.set curr_scroll_state state
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
                     navigate z_patches Next;
                     `Handled
                 | `ASCII 'p', [] ->
                     navigate z_patches Prev;
                     `Handled
                 | `ASCII 'h', [] ->
                     Lwd.set help true;
                     `Handled
                 | `ASCII 't', [] ->
                     toggle_view_mode ();
                     `Handled
                 | `ASCII 'l', [] ->
                     toggle_line_numbers ();
                     `Handled
                 | _ -> `Unhandled)
               (W.string
                  "Type 'h' to go to the help panel, 'q' to quit, 'n' to go to \
                   the next operation, 'p' to go to the previous operation. \
                   Press 't' to toggle view mode, 'l' to toggle line numbers.");
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
