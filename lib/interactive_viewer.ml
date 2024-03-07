open Nottui
module W = Nottui_widgets
open Lwd_infix

let operation_info z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  W.string
    ~attr:Notty.A.(fg lightcyan)
    (Printf.sprintf "Operation %d of %d"
       (Zipper.get_current_index z + 1)
       (Zipper.get_total_length z))

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

(* let pure_str s = Lwd.pure (W.string s) *)
let quit = Lwd.var false

let split_hunk hunk =
  let mine, their =
    List.fold_left
      (fun (mine_acc, their_acc) line ->
        match line with
        | `Common s -> (mine_acc @ [ `Common s ], their_acc @ [ `Common s ])
        | `Mine s -> (mine_acc @ [ `Mine s ], their_acc)
        | `Their s -> (mine_acc, their_acc @ [ `Their s ]))
      ([], []) hunk.Patch.lines
  in
  (mine, their)

let lines_to_ui_with_numbers lines attr_line_number attr_change =
  List.mapi
    (fun index line ->
      let ui_line, attr =
        match line with
        | `Common s ->
            (s, attr_line_number) (* Common lines use the line number color *)
        | `Mine s -> (s, attr_change) (* Mine lines use the deletion color *)
        | `Their s -> (s, attr_change)
        (* Their lines use the addition color *)
      in
      Ui.hcat
        [
          W.string ~attr:attr_line_number (Printf.sprintf "%4d " (index + 1));
          W.string ~attr ui_line;
        ])
    lines

let ui_of_hunk_side_by_side hunk =
  let mine_lines, their_lines = split_hunk hunk in

  let attr_line_number = Notty.A.(fg lightblue) in
  let attr_mine = Notty.A.(fg red ++ st bold) in
  let attr_their = Notty.A.(fg green ++ st bold) in

  let separator = W.string ~attr:attr_line_number "|" in

  (* For mine lines, we use the deletion color, and for their lines, the addition color *)
  let mine_ui =
    lines_to_ui_with_numbers mine_lines attr_line_number attr_mine
  in
  let their_ui =
    lines_to_ui_with_numbers their_lines attr_line_number attr_their
  in

  let space = Ui.space 1 0 in
  (* Adding a visual gap *)
  Ui.hcat [ Ui.vcat mine_ui; space; separator; space; Ui.vcat their_ui ]

let current_hunks_side_by_side z_patches : ui Lwd.t =
  let$ z = Lwd.get z_patches in
  let p = Zipper.get_focus z in
  let hunks_ui = List.map ui_of_hunk_side_by_side p.Patch.hunks in
  Ui.vcat @@ hunks_ui

type view_mode = SideBySide | Regular

let view_mode = Lwd.var SideBySide

let toggle_view_mode () =
  match Lwd.peek view_mode with
  | Regular -> Lwd.set view_mode SideBySide
  | SideBySide -> Lwd.set view_mode Regular

let view (patches : Patch.t list) =
  let z_patches : 'a Zipper.t Lwd.var =
    match Zipper.zipper_of_list patches with
    | Some z -> Lwd.var z
    | None -> failwith "zipper_of_list: empty list"
  in
  let hunks_ui =
    Lwd.bind (Lwd.get view_mode) ~f:(fun mode ->
        match mode with
        | Regular -> current_hunks z_patches
        | SideBySide -> current_hunks_side_by_side z_patches)
  in
  W.vbox
    [
      operation_info z_patches;
      current_operation z_patches;
      W.scrollbox hunks_ui;
      (* This now dynamically updates based on view_mode *)
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
             | `ASCII 't', [] ->
                 toggle_view_mode ();
                 `Handled
             | _ -> `Unhandled)
           (W.string
              "Type 'q' to quit, 'n' to go to the next operation, 'p' to go to \
               the previous operation, 't' to toggle view mode");
    ]

let start patch = Ui_loop.run ~quit ~tick_period:0.2 (view patch)
