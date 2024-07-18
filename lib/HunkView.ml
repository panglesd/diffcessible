open Nottui
module W = Nottui_widgets

(* Common Functions *)

type line =
  | Change of string * WordDiff.word_diff list
  | Common of string
  | Empty

let line_to_string = function Change (s, _) -> s | Common s -> s | Empty -> ""

let construct_hunk mine their =
  let rec aux acc mine_lines their_lines =
    match (mine_lines, their_lines) with
    | [], [] -> List.rev acc
    | Common s1 :: rest1, Common s2 :: rest2 when s1 = s2 ->
        aux (Common s1 :: acc) rest1 rest2
    | Change (s1, _) :: rest1, Change (s2, _) :: rest2 ->
        let word_diff = WordDiff.apply_word_diff s1 s2 in
        aux (Change (s1, word_diff) :: acc) rest1 rest2
    | Empty :: rest1, Empty :: rest2 -> aux (Empty :: acc) rest1 rest2
    | l1 :: rest1, l2 :: rest2 ->
        let s1 = line_to_string l1 in
        let s2 = line_to_string l2 in
        let word_diff = WordDiff.apply_word_diff s1 s2 in
        aux (Change (s1, word_diff) :: acc) rest1 rest2
    | [], their_lines ->
        let mapped_lines =
          List.map
            (function
              | Common s ->
                  Change (s, [ WordDiff.WAdded (WordDiff.string_to_words s) ])
              | Change (s, _) ->
                  Change (s, [ WordDiff.WAdded (WordDiff.string_to_words s) ])
              | l -> l)
            their_lines
        in
        List.rev_append acc mapped_lines
    | mine_lines, [] ->
        let mapped_lines =
          List.map
            (function
              | Common s ->
                  Change (s, [ WordDiff.WDeleted (WordDiff.string_to_words s) ])
              | Change (s, _) ->
                  Change (s, [ WordDiff.WDeleted (WordDiff.string_to_words s) ])
              | l -> l)
            mine_lines
        in
        List.rev_append acc mapped_lines
  in
  aux [] mine their

let split_and_align_hunk hunks : line list * line list =
  let rec process_hunk mine_acc their_acc = function
    | [] -> (List.rev mine_acc, List.rev their_acc)
    | `Common line :: rest ->
        process_hunk (Common line :: mine_acc) (Common line :: their_acc) rest
    | changes ->
        let rec buffer_changes mine their = function
          | `Mine line :: rest ->
              buffer_changes (Change (line, []) :: mine) their rest
          | `Their line :: rest ->
              buffer_changes mine (Change (line, []) :: their) rest
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

let process_line (hunk : Patch.hunk) i line =
  let line_number =
    W.string
      ~attr:Notty.A.(fg yellow)
      (Printf.sprintf "%4d " (succ (hunk.Patch.mine_start + i)))
  in
  match line with
  | Common s ->
      let content = W.string s in
      Ui.hcat [ line_number; content ]
  | Change (_, word_diffs) ->
      let word_uis =
        List.map
          (function
            | WordDiff.WDeleted words ->
                W.string
                  ~attr:Notty.A.(bg red ++ fg white)
                  (WordDiff.words_to_string words)
            | WordDiff.WAdded words ->
                W.string
                  ~attr:Notty.A.(bg green ++ fg black)
                  (WordDiff.words_to_string words)
            | WordDiff.WEqual words -> W.string (WordDiff.words_to_string words))
          word_diffs
      in
      let space = W.string " " in
      let content =
        Ui.hcat
          (List.fold_left (fun acc ui -> ui :: space :: acc) [] word_uis
          |> List.rev)
      in
      Ui.hcat [ line_number; content ]
  | Empty -> W.string ""

let rec process_lines hunk i acc = function
  | [] -> List.rev acc
  | line :: rest ->
      let line_ui = process_line hunk i line in
      process_lines hunk (succ i) (line_ui :: acc) rest

let ui_unified_diff (hunk : Patch.hunk) : Nottui.ui =
  let mine, their = split_and_align_hunk hunk.Patch.lines in
  let constructed_hunk = construct_hunk mine their in

  let body = Ui.vcat (process_lines hunk 0 [] constructed_hunk) in
  let summary = ui_hunk_summary hunk in

  Ui.vcat [ summary; body ]

let current_hunks (z_patches : Patch.t Zipper.t) : Nottui.ui =
  let p = Zipper.get_focus z_patches in
  let hunks = List.map ui_unified_diff p.Patch.hunks in
  Ui.vcat hunks

(** Side by side diff view implementation **)

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
          | Change (s, _) ->
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
