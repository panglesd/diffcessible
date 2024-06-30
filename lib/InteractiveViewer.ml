open Nottui
module W = Nottui_widgets
open Lwd_infix
open HelpView
(* Summary of operations *)

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

let current_operation z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  ui_of_operation p.Patch.operation

type direction = Prev | Next

let navigate z_patches (dir : direction) : unit =
  let z = Lwd.peek z_patches in
  match dir with
  | Prev -> Lwd.set z_patches (Zipper.prev z)
  | Next -> Lwd.set z_patches (Zipper.next z)

let quit = Lwd.var false

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

(* Implementation of Single View Mode *)

let ui_hunk_summary hunk =
  let mine_info =
    if hunk.Patch.mine_len = 0 then "0,0"
    else Printf.sprintf "%d,%d" (hunk.Patch.mine_start + 1) hunk.Patch.mine_len
  in
  let their_info =
    if hunk.Patch.their_len = 0 then "0,0"
    else
      Printf.sprintf "%d,%d" (hunk.Patch.their_start + 1) hunk.Patch.their_len
  in
  let mine_summary =
    W.string ~attr:Notty.A.(fg red) (Printf.sprintf "-%s" mine_info)
  in
  let their_summary =
    W.string ~attr:Notty.A.(fg green) (Printf.sprintf "+%s" their_info)
  in
  let at_symbols = W.string ~attr:Notty.A.(fg lightblue) "@@" in
  Ui.hcat
    [
      at_symbols;
      W.string " ";
      mine_summary;
      W.string " ";
      their_summary;
      W.string " ";
      at_symbols;
    ]

let ui_unified_diff hunk =
  let their_line_num = ref hunk.Patch.their_start in
  let mine_line_num = ref hunk.Patch.mine_start in
  let lines_ui =
    List.map
      (function
        | `Common line ->
            incr their_line_num;
            incr mine_line_num;
            W.string ~attr:Notty.A.empty
              (Printf.sprintf "%2d %2d   %s" !mine_line_num !their_line_num line)
        | `Their line ->
            incr their_line_num;
            W.string
              ~attr:Notty.A.(fg green)
              (Printf.sprintf "   %2d + %s" !their_line_num line)
        | `Mine line ->
            incr mine_line_num;
            W.string
              ~attr:Notty.A.(fg red)
              (Printf.sprintf "%2d    - %s" !mine_line_num line))
      hunk.Patch.lines
    |> Ui.vcat
  in
  Ui.vcat [ ui_hunk_summary hunk; lines_ui ]

let current_hunks z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks = List.map ui_unified_diff p.Patch.hunks in
  Ui.vcat hunks

(** Side by side diff view implementation **)

type view_mode = SideBySide | Normal

let view_mode = Lwd.var Normal

let toggle_view_mode () =
  match Lwd.peek view_mode with
  | Normal -> Lwd.set view_mode SideBySide
  | SideBySide -> Lwd.set view_mode Normal

type line = Change of string | Common of string | Empty

let rec split_and_align_hunk hunks mine_acc their_acc =
  let rec buffer_changes temp_mine temp_their hunks =
    match hunks with
    | [] -> (temp_mine, temp_their, hunks)
    | `Common _ :: _ -> (temp_mine, temp_their, hunks)
    | `Mine line :: t -> buffer_changes (Change line :: temp_mine) temp_their t
    | `Their line :: t -> buffer_changes temp_mine (Change line :: temp_their) t
  in
  let append_balanced temp_mine temp_their =
    let mine_len = List.length temp_mine in
    let their_len = List.length temp_their in
    let fill_empty n = List.init n (fun _ -> Empty) in

    let diff = mine_len - their_len in
    let empty_list = fill_empty (abs diff) in

    if diff > 0 then (temp_mine, empty_list @ temp_their)
    else if diff < 0 then (empty_list @ temp_mine, temp_their)
    else (temp_mine, temp_their)
  in

  match hunks with
  | [] -> (List.rev mine_acc, List.rev their_acc)
  | _ -> (
      let temp_mine, temp_their, remaining_hunks = buffer_changes [] [] hunks in
      let balanced_mine, balanced_their =
        append_balanced temp_mine temp_their
      in
      let updated_mine_acc = balanced_mine @ mine_acc in
      let updated_their_acc = balanced_their @ their_acc in
      match remaining_hunks with
      | `Common line :: t ->
          let common_mine_acc = Common line :: updated_mine_acc in
          let common_their_acc = Common line :: updated_their_acc in
          split_and_align_hunk t common_mine_acc common_their_acc
      | _ ->
          split_and_align_hunk remaining_hunks updated_mine_acc
            updated_their_acc)

let lines_with_numbers lines attr_change prefix =
  let line_num = ref 0 in
  List.fold_left
    (fun acc line ->
      match line with
      | Common s ->
          incr line_num;
          let content = Printf.sprintf "%3d   %s" !line_num s in
          (content, Notty.A.empty) :: acc
      | Change s ->
          incr line_num;
          let content = Printf.sprintf "%3d %s %s" !line_num prefix s in
          (content, attr_change) :: acc
      | Empty ->
          let content = Printf.sprintf "      " in
          (content, Notty.A.empty) :: acc)
    [] lines
  |> List.rev
  |> List.map (fun (content, attr) -> W.string ~attr content)

let create_summary start_line_num hunk_length attr change_type =
  let sign = match change_type with `Add -> "+" | `Remove -> "-" in
  if hunk_length > 0 then
    W.string ~attr
      (Printf.sprintf "@@ %s%d,%d @@" sign start_line_num hunk_length)
  else W.string ~attr (Printf.sprintf "@@ %s0,0 @@" sign)

let ui_of_hunk_side_by_side hunk =
  let attr_mine = Notty.A.(fg red ++ st bold) in
  let attr_their = Notty.A.(fg green ++ st bold) in

  let mine_lines, their_lines = split_and_align_hunk hunk.Patch.lines [] [] in

  let content_mine = lines_with_numbers mine_lines attr_mine "-" in
  let content_their = lines_with_numbers their_lines attr_their "+" in
  let summary_mine =
    create_summary
      (hunk.Patch.mine_start + 1)
      hunk.Patch.mine_len attr_mine `Remove
  in
  let summary_their =
    create_summary
      (hunk.Patch.their_start + 1)
      hunk.Patch.their_len attr_their `Add
  in
  let space = Ui.space 1 0 in
  Ui.hcat
    [
      Ui.resize ~w:0 ~sw:2 (Ui.vcat (summary_mine :: content_mine));
      space;
      Ui.resize ~w:0 ~sw:2 (Ui.vcat (summary_their :: content_their));
    ]

let current_hunks_side_by_side z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks_ui = List.map (fun h -> ui_of_hunk_side_by_side h) p.Patch.hunks in
  Ui.vcat @@ hunks_ui

(** end of side by side diff view implementation **)

let view (patches : Patch.t list) =
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
    let$* help_visible = Lwd.get help_visible in
    if help_visible then
      W.vbox
        [
          W.scrollbox @@ Lwd.pure @@ help_panel;
          (* Scrollable help panel *)
          Lwd.pure
          @@ Ui.keyboard_area
               (function
                 | `ASCII 'q', [] ->
                     toggle_help_visibility ();
                     (* Correct variable name *)
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
  W.vbox [ ui ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)

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
