open Nottui
module W = Nottui_widgets

let operation_info (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  let num_hunks = List.length p.Patch.hunks in
  let hunk_text =
    match num_hunks with
    | 1 -> "1 hunk"
    | _ -> Printf.sprintf "%d hunks" num_hunks
  in
  W.string
    ~attr:Notty.A.(fg lightcyan)
    (Printf.sprintf "Operation %d of %d, %s"
       (Zipper.get_current_index z_patches + 1)
       (Zipper.get_total_length z_patches)
       hunk_text)

let ui_of_operation (operation : Patch.operation) : Nottui.ui =
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

let current_operation (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  ui_of_operation p.Patch.operation

let additions_and_removals lines : int * int =
  let add_line (additions, removals) line =
    match line with
    | `Their _ -> (additions + 1, removals)
    | `Mine _ -> (additions, removals + 1)
    | `Common _ -> (additions, removals)
  in
  List.fold_left add_line (0, 0) lines

let accumulate_count (hunks : Patch.hunk list) : int * int =
  List.fold_left
    (fun (add_acc, remove_acc) hunk ->
      let add_in_hunk, remove_in_hunk =
        additions_and_removals hunk.Patch.lines
      in
      (add_acc + add_in_hunk, remove_acc + remove_in_hunk))
    (0, 0) hunks

let change_summary (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
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
