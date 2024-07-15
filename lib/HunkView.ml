open Nottui
module W = Nottui_widgets
module WordDiff = Simple_diff.Make (String)

type line = Change of string | Common of string | Empty

type word_diff =
  | CommonWord of string
  | AddedWord of string
  | DeletedWord of string

let split_into_words (line : string) : string array =
  let words = String.split_on_char ' ' line in
  Array.of_list words

let word_diff (s1 : string) (s2 : string) : WordDiff.diff list =
  let words1 = split_into_words s1 in
  let words2 = split_into_words s2 in
  WordDiff.get_diff words1 words2

let lcs xs' ys' =
  let xs = Array.of_list xs' and ys = Array.of_list ys' in
  let n = Array.length xs and m = Array.length ys in
  let a = Array.make_matrix (n + 1) (m + 1) [] in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      a.(i).(j) <-
        (if xs.(i) = ys.(j) then xs.(i) :: a.(i + 1).(j + 1)
         else if List.length a.(i).(j + 1) > List.length a.(i + 1).(j) then
           a.(i).(j + 1)
         else a.(i + 1).(j))
    done
  done;
  a.(0).(0)

let pad_and_append (orig_acc : line list) (changes : line list) (max_len : int)
    : line list =
  let rec pad_append (acc : line list) (i : int) : line list =
    if i < max_len then
      if i < List.length changes then
        pad_append (List.nth changes i :: acc) (i + 1)
      else pad_append (Empty :: acc) (i + 1)
    else acc
  in
  pad_append orig_acc 0

let split_and_align_hunk_diff (hunks : WordDiff.diff list) :
    line list * line list =
  let rec process_hunk (mine_acc : line list) (their_acc : line list) = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | WordDiff.Equal line :: rest ->
        let common_line = String.concat " " (Array.to_list line) in
        process_hunk
          (Common common_line :: mine_acc)
          (Common common_line :: their_acc)
          rest
    | changes ->
        let rec buffer_changes (mine : line list) (their : line list) = function
          | WordDiff.Added line :: rest ->
              let added_line = String.concat " " (Array.to_list line) in
              buffer_changes (Change added_line :: mine) their rest
          | WordDiff.Deleted line :: rest ->
              let deleted_line = String.concat " " (Array.to_list line) in
              buffer_changes mine (Change deleted_line :: their) rest
          | remaining -> (List.rev mine, List.rev their, remaining)
        in
        let mine_changes, their_changes, rest = buffer_changes [] [] changes in
        let max_len =
          max (List.length mine_changes) (List.length their_changes)
        in
        let new_mine_acc = pad_and_append mine_acc mine_changes max_len in
        let new_their_acc = pad_and_append their_acc their_changes max_len in
        process_hunk new_mine_acc new_their_acc rest
  in
  process_hunk [] [] hunks

let line_to_string = function Change s | Common s -> s | Empty -> ""

let word_diff_to_ui (diff : word_diff) : Nottui.ui =
  let attr, word =
    match diff with
    | CommonWord w -> (Notty.A.empty, w)
    | AddedWord w -> (Notty.A.(fg green), w)
    | DeletedWord w -> (Notty.A.(fg red), w)
  in
  W.string ~attr (word ^ " ")

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

(* let ui_unified_diff (hunk : Patch.hunk) : Nottui.ui = *)
(*   let rec process_lines (mine_num : int) (their_num : int) *)
(*       (acc : Nottui.ui list) = function *)
(*     | [] -> List.rev acc *)
(*     | line :: rest -> *)
(*         let new_mine, new_their, ui_element = *)
(*           match line with *)
(*           | `Common line -> *)
(*               let ui = *)
(*                 W.string ~attr:Notty.A.empty *)
(*                   (Printf.sprintf "%2d %2d   %s" (mine_num + 1) (their_num + 1) *)
(*                      line) *)
(*               in *)
(*               (mine_num + 1, their_num + 1, ui) *)
(*           | `Their line -> *)
(*               let ui = *)
(*                 W.string *)
(*                   ~attr:Notty.A.(fg green) *)
(*                   (Printf.sprintf "   %2d + %s" (their_num + 1) line) *)
(*               in *)
(*               (mine_num, their_num + 1, ui) *)
(*           | `Mine line -> *)
(*               let ui = *)
(*                 W.string *)
(*                   ~attr:Notty.A.(fg red) *)
(*                   (Printf.sprintf "%2d    - %s" (mine_num + 1) line) *)
(*               in *)
(*               (mine_num + 1, their_num, ui) *)
(*         in *)
(*         process_lines new_mine new_their (ui_element :: acc) rest *)
(*   in *)
(**)
(*   let lines_ui = *)
(*     process_lines hunk.Patch.mine_start hunk.Patch.their_start [] *)
(*       hunk.Patch.lines *)
(*   in *)
(*   let lines_ui_vcat = Ui.vcat lines_ui in *)
(**)
(*   Ui.vcat [ ui_hunk_summary hunk; lines_ui_vcat ] *)
(**)
(* let current_hunks (z_patches : Patch.t Zipper.t) : Nottui.ui = *)
(*   let p = Zipper.get_focus z_patches in *)
(*   let hunks = List.map ui_unified_diff p.Patch.hunks in *)
(*   Ui.vcat hunks *)

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
