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

let line_to_string = function Change s -> s | Common s -> s | Empty -> ""

let split_and_align_hunk hunks : line list * line list =
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
        let rec pad_and_append orig_acc changes i =
          if i < max_len then
            if i < List.length changes then
              pad_and_append (List.nth changes i :: orig_acc) changes (i + 1)
            else pad_and_append (Empty :: orig_acc) changes (i + 1)
          else orig_acc
        in
        let new_mine_acc = pad_and_append mine_acc mine_changes 0 in
        let new_their_acc = pad_and_append their_acc their_changes 0 in
        process_hunk new_mine_acc new_their_acc rest
  in
  process_hunk [] [] hunks

let string_to_words s = Array.of_list (String.split_on_char ' ' s)
let words_to_string words = String.concat " " (Array.to_list words)

let diff_words s1 s2 =
  let words1 = string_to_words s1 in
  let words2 = string_to_words s2 in
  WordDiff.WordDiff.get_diff words1 words2

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

let apply_word_diff s1 s2 =
  let diff = diff_words s1 s2 in
  List.map
    (function
      | WordDiff.WordDiff.Deleted words -> WDeleted words
      | WordDiff.WordDiff.Added words -> WAdded words
      | WordDiff.WordDiff.Equal words -> WEqual words)
    diff

(* UI functions *)

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

let word_to_ui word attr = W.string ~attr (word ^ " ")

let line_to_ui mine_num their_num line =
  match line with
  | Common text ->
      let ui =
        W.string ~attr:Notty.A.empty
          (Printf.sprintf "%2d %2d   %s" (mine_num + 1) (their_num + 1) text)
      in
      (mine_num + 1, their_num + 1, ui)
  | Change text ->
      let mine_ui =
        W.string
          ~attr:Notty.A.(fg red)
          (Printf.sprintf "%2d    - %s" (mine_num + 1) text)
      in
      let their_ui =
        W.string
          ~attr:Notty.A.(fg green)
          (Printf.sprintf "   %2d + %s" (their_num + 1) text)
      in
      (mine_num + 1, their_num + 1, Ui.vcat [ mine_ui; their_ui ])
  | Empty -> (mine_num, their_num, W.string "")

let process_lines hunk =
  let mine, their = split_and_align_hunk hunk.Patch.lines in
  let rec aux mine_num their_num acc = function
    | [], [] -> List.rev acc
    | m :: m_rest, t :: t_rest ->
        let new_mine, new_their, ui_element =
          match (m, t) with
          | Common m_text, Common t_text when m_text = t_text ->
              line_to_ui mine_num their_num (Common m_text)
          | Change m_text, Change t_text ->
              let diff = apply_word_diff m_text t_text in
              let mine_ui =
                Ui.hcat
                  (List.map
                     (function
                       | WDeleted words ->
                           word_to_ui (words_to_string words) Notty.A.(fg red)
                       | WEqual words ->
                           word_to_ui (words_to_string words) Notty.A.empty
                       | WAdded _ -> Ui.empty)
                     diff)
              in
              let their_ui =
                Ui.hcat
                  (List.map
                     (function
                       | WAdded words ->
                           word_to_ui (words_to_string words) Notty.A.(fg green)
                       | WEqual words ->
                           word_to_ui (words_to_string words) Notty.A.empty
                       | WDeleted _ -> Ui.empty)
                     diff)
              in
              ( mine_num + 1,
                their_num + 1,
                Ui.vcat
                  [
                    Ui.hcat
                      [
                        W.string
                          ~attr:Notty.A.(fg red)
                          (Printf.sprintf "%2d    - " (mine_num + 1));
                        mine_ui;
                      ];
                    Ui.hcat
                      [
                        W.string
                          ~attr:Notty.A.(fg green)
                          (Printf.sprintf "   %2d + " (their_num + 1));
                        their_ui;
                      ];
                  ] )
          | _ -> line_to_ui mine_num their_num m
        in
        aux new_mine new_their (ui_element :: acc) (m_rest, t_rest)
    | _, _ ->
        List.rev acc (* This case should not happen if aligned correctly *)
  in
  aux hunk.Patch.mine_start hunk.Patch.their_start [] (mine, their)

let ui_unified_diff (hunk : Patch.hunk) : Nottui.ui =
  let lines_ui = process_lines hunk in
  let lines_ui_vcat = Ui.vcat lines_ui in
  Ui.vcat [ ui_hunk_summary hunk; lines_ui_vcat ]

let current_hunks (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  let hunks = List.map ui_unified_diff p.Patch.hunks in
  Ui.vcat hunks

(** Side by side diff view implementation **)

let create_summary (start_line_num : int) (hunk_length : int)
    (attr : Notty.attr) (change_type : [ `Add | `Remove ]) : Nottui.ui =
  let sign = match change_type with `Add -> "+" | `Remove -> "-" in
  if hunk_length > 0 then
    W.string ~attr
      (Printf.sprintf "@@ %s%d,%d @@" sign start_line_num hunk_length)
  else W.string ~attr (Printf.sprintf "@@ %s0,0 @@" sign)

let lines_with_numbers (lines : line list) (attr_change : Notty.attr)
    (prefix : string) : Nottui.ui list =
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
