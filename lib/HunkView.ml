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
  let rec process_lines mine_num their_num acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let new_mine, new_their, ui_element =
          match line with
          | `Common line ->
              let ui =
                W.string ~attr:Notty.A.empty
                  (Printf.sprintf "%2d %2d   %s" (mine_num + 1) (their_num + 1)
                     line)
              in
              (mine_num + 1, their_num + 1, ui)
          | `Their line ->
              let ui =
                W.string
                  ~attr:Notty.A.(fg green)
                  (Printf.sprintf "   %2d + %s" (their_num + 1) line)
              in
              (mine_num, their_num + 1, ui)
          | `Mine line ->
              let ui =
                W.string
                  ~attr:Notty.A.(fg red)
                  (Printf.sprintf "%2d    - %s" (mine_num + 1) line)
              in
              (mine_num + 1, their_num, ui)
        in
        process_lines new_mine new_their (ui_element :: acc) rest
  in

  let lines_ui =
    process_lines hunk.Patch.mine_start hunk.Patch.their_start []
      hunk.Patch.lines
  in
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

let split_and_align_hunk hunks =
  let rec process_hunk mine_acc their_acc = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | `Common line :: rest ->
        process_hunk (Common line :: mine_acc) (Common line :: their_acc) rest
    | changes ->
        let rec buffer_changes mine their = function
          | `Mine line :: rest ->
              buffer_changes (Change line :: mine) their rest
          | `Their line :: rest ->
              buffer_changes mine (Change line :: their) rest
          | remaining -> (List.rev mine, List.rev their, remaining)
        in
        let mine_changes, their_changes, rest = buffer_changes [] [] changes in
        let max_len =
          max (List.length mine_changes) (List.length their_changes)
        in
        let pad_and_append orig_acc changes =
          let rec pad_append acc i =
            if i < max_len then
              if i < List.length changes then
                pad_append (List.nth changes i :: acc) (i + 1)
              else pad_append (Empty :: acc) (i + 1)
            else acc
          in
          pad_append orig_acc 0
        in
        let new_mine_acc = pad_and_append mine_acc mine_changes in
        let new_their_acc = pad_and_append their_acc their_changes in
        process_hunk new_mine_acc new_their_acc rest
  in
  process_hunk [] [] hunks

let lines_with_numbers lines attr_change prefix =
  let rec process_lines line_num acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let content, attr, next_num =
          match line with
          | Common s ->
              let content = Printf.sprintf "%3d   %s" line_num s in
              (content, Notty.A.empty, line_num + 1)
          | Change s ->
              let content = Printf.sprintf "%3d %s %s" line_num prefix s in
              (content, attr_change, line_num + 1)
          | Empty ->
              let content = Printf.sprintf "      " in
              (content, Notty.A.empty, line_num)
        in
        let new_acc = W.string ~attr content :: acc in
        process_lines next_num new_acc rest
  in
  process_lines 1 [] lines

let create_summary start_line_num hunk_length attr change_type =
  let sign = match change_type with `Add -> "+" | `Remove -> "-" in
  if hunk_length > 0 then
    W.string ~attr
      (Printf.sprintf "@@ %s%d,%d @@" sign start_line_num hunk_length)
  else W.string ~attr (Printf.sprintf "@@ %s0,0 @@" sign)

let ui_of_hunk_side_by_side hunk =
  let attr_mine = Notty.A.(fg red ++ st bold) in
  let attr_their = Notty.A.(fg green ++ st bold) in

  let mine_lines, their_lines = split_and_align_hunk hunk.Patch.lines in

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
