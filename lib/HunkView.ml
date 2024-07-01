open Nottui
module W = Nottui_widgets
open Lwd_infix

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
  let initial_line_nums = (hunk.Patch.mine_start, hunk.Patch.their_start) in

  let update_lines (mine_line_num, their_line_num) = function
    | `Common line ->
        (mine_line_num + 1, their_line_num + 1),
        W.string ~attr:Notty.A.empty
          (Printf.sprintf "%2d %2d   %s" (mine_line_num + 1) (their_line_num + 1) line)
    | `Their line ->
        (mine_line_num, their_line_num + 1),
        W.string ~attr:Notty.A.(fg green)
          (Printf.sprintf "   %2d + %s" (their_line_num + 1) line)
    | `Mine line ->
        (mine_line_num + 1, their_line_num),
        W.string ~attr:Notty.A.(fg red)
          (Printf.sprintf "%2d    - %s" (mine_line_num + 1) line)
  in
  (* _ is the final state of the lines, if needed later *)
  let _, lines_ui = List.fold_left_map update_lines initial_line_nums hunk.Patch.lines in 
  let lines_ui_vcat = Ui.vcat lines_ui in

  Ui.vcat [ ui_hunk_summary hunk; lines_ui_vcat ]

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
