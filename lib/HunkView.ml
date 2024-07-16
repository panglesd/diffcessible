open Nottui
module W = Nottui_widgets

(* Common Functions *)

type diff = Equal | Added | Deleted
type word = diff * string

type assembled_line =
  | CommonWord of word list
  | MineWord of word list
  | TheirWord of word list

type hunk = assembled_line list
type line = Change of string | Common of string | Empty

let collect_lines hunks : line list * line list =
  let rec process mine_acc their_acc = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | `Mine line :: rest -> process (Change line :: mine_acc) their_acc rest
    | `Their line :: rest -> process mine_acc (Change line :: their_acc) rest
    | `Common _ :: rest -> process mine_acc their_acc rest
  in
  process [] [] hunks

let line_to_words (line : line) : string list =
  match line with
  | Change s | Common s -> String.split_on_char ' ' s
  | Empty -> []

let lines_to_words (lines : line list) : string list =
  List.concat_map line_to_words lines

(* Word diffing module *)
module WordDiff = struct
  type t = string

  let compare = String.compare
end

module Simple_diff_word = Simple_diff.Make (WordDiff)

let diff_to_assembled_lines (diff : Simple_diff_word.diff list) :
    assembled_line list =
  let rec process acc = function
    | [] -> List.rev acc
    | Simple_diff_word.Equal words :: rest ->
        let common =
          CommonWord (Array.to_list words |> List.map (fun w -> (Equal, w)))
        in
        process (common :: acc) rest
    | Simple_diff_word.Deleted words :: rest ->
        let mine =
          MineWord (Array.to_list words |> List.map (fun w -> (Deleted, w)))
        in
        process (mine :: acc) rest
    | Simple_diff_word.Added words :: rest ->
        let their =
          TheirWord (Array.to_list words |> List.map (fun w -> (Added, w)))
        in
        process (their :: acc) rest
  in
  process [] diff

let process_hunk (hunk : Patch.hunk) : hunk =
  let mine_lines, their_lines = collect_lines hunk.Patch.lines in
  let mine_words = lines_to_words mine_lines in
  let their_words = lines_to_words their_lines in

  let diff =
    Simple_diff_word.get_diff (Array.of_list mine_words)
      (Array.of_list their_words)
  in

  diff_to_assembled_lines diff

let ui_hunk_summary (hunk : Patch.hunk) : Nottui.ui =
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

module A = Notty.A

(* Helper function to create a styled word *)
let styled_word (d, w) =
  match d with
  | Deleted -> W.string ~attr:A.(fg red ++ st bold) w
  | Added -> W.string ~attr:A.(fg green ++ st bold) w
  | Equal -> W.string w

(* Helper function to create a line number *)
let line_number n attr = W.string ~attr (Printf.sprintf "%3d " n)

(* Helper function to create a line prefix *)
let line_prefix c attr = W.string ~attr (Printf.sprintf "%c " c)

let ui_unified_diff (hunk : Patch.hunk) : Nottui.ui =
  let processed_hunk = process_hunk hunk in
  let rec render_lines (mine_num : int) (their_num : int) (acc : Nottui.ui list)
      = function
    | [] -> List.rev acc
    | CommonWord words :: rest ->
        let line = Ui.hcat (List.map styled_word words) in
        let ui =
          Ui.hcat
            [
              line_number mine_num A.empty;
              line_number their_num A.empty;
              line_prefix ' ' A.empty;
              line;
            ]
        in
        render_lines (mine_num + 1) (their_num + 1) (ui :: acc) rest
    | MineWord words :: TheirWord their_words :: rest ->
        let mine_line = Ui.hcat (List.map styled_word words) in
        let their_line = Ui.hcat (List.map styled_word their_words) in
        let mine_ui =
          Ui.hcat
            [
              line_number mine_num A.(fg red);
              line_number their_num A.empty;
              line_prefix '-' A.(fg red);
              mine_line;
            ]
        in
        let their_ui =
          Ui.hcat
            [
              line_number mine_num A.empty;
              line_number (their_num + 1) A.(fg green);
              line_prefix '+' A.(fg green);
              their_line;
            ]
        in
        render_lines (mine_num + 1) (their_num + 1)
          (their_ui :: mine_ui :: acc)
          rest
    | MineWord words :: rest ->
        let line = Ui.hcat (List.map styled_word words) in
        let ui =
          Ui.hcat
            [
              line_number mine_num A.(fg red);
              line_number their_num A.empty;
              line_prefix '-' A.(fg red);
              line;
            ]
        in
        render_lines (mine_num + 1) their_num (ui :: acc) rest
    | TheirWord words :: rest ->
        let line = Ui.hcat (List.map styled_word words) in
        let ui =
          Ui.hcat
            [
              line_number mine_num A.empty;
              line_number their_num A.(fg green);
              line_prefix '+' A.(fg green);
              line;
            ]
        in
        render_lines mine_num (their_num + 1) (ui :: acc) rest
  in
  let lines_ui =
    render_lines hunk.Patch.mine_start hunk.Patch.their_start [] processed_hunk
  in
  let lines_ui_vcat = Ui.vcat lines_ui in
  Ui.vcat [ ui_hunk_summary hunk; lines_ui_vcat ]

let current_hunks (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  let hunks = List.map ui_unified_diff p.Patch.hunks in
  Ui.vcat hunks

(** Side by side diff view implementation **)

let split_and_align_hunk hunks : line list * line list =
  let rec process_hunk (mine_acc : line list) (their_acc : line list) = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | `Common line :: rest ->
        process_hunk (Common line :: mine_acc) (Common line :: their_acc) rest
    | changes ->
        let rec buffer_changes (mine : line list) (their : line list) = function
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
        let pad_and_append (orig_acc : line list) (changes : line list) :
            line list =
          let rec pad_append (acc : line list) (i : int) : line list =
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

let lines_with_numbers (lines : line list) (attr_change : Notty.attr)
    (prefix : string) : Nottui.ui list =
  let rec process_lines (line_num : int) (acc : Nottui.ui list) = function
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

let create_summary (start_line_num : int) (hunk_length : int)
    (attr : Notty.attr) (change_type : [ `Add | `Remove ]) : Nottui.ui =
  let sign = match change_type with `Add -> "+" | `Remove -> "-" in
  if hunk_length > 0 then
    W.string ~attr
      (Printf.sprintf "@@ %s%d,%d @@" sign start_line_num hunk_length)
  else W.string ~attr (Printf.sprintf "@@ %s0,0 @@" sign)

let ui_of_hunk_side_by_side (hunk : Patch.hunk) : Nottui.ui =
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

let current_hunks_side_by_side (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  let hunks_ui = List.map ui_of_hunk_side_by_side p.Patch.hunks in
  Ui.vcat hunks_ui
