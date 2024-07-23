open Nottui
module W = Nottui_widgets

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

type line_change =
  | Common of string
  | Added of string
  | Deleted of string
  | Modified of word_diff list

type hunk = {
  mine_start : int;
  mine_len : int;
  their_start : int;
  their_len : int;
  lines : line_change list;
}

let string_to_words s = Array.of_list (String.split_on_char ' ' s)
let words_to_string words = String.concat " " (Array.to_list words)

module Diff = struct
  include Simple_diff.Make (String)

  let diff_words s1 s2 =
    let words1 = string_to_words s1 in
    let words2 = string_to_words s2 in
    get_diff words1 words2

  let apply_word_diff s1 s2 =
    let diff = diff_words s1 s2 in
    List.map
      (function
        | Deleted words -> WDeleted words
        | Added words -> WAdded words
        | Equal words -> WEqual words)
      diff
end

let compute (patch_hunk : Patch.hunk) : hunk =
  let rec process_changes acc = function
    | [] -> List.rev acc
    | `Mine m :: `Their t :: rest ->
        process_changes (Modified (Diff.apply_word_diff m t) :: acc) rest
    | `Their t :: rest -> process_changes (Added t :: acc) rest
    | `Mine m :: rest -> process_changes (Deleted m :: acc) rest
    | `Common c :: rest -> process_changes (Common c :: acc) rest
  in
  {
    mine_start = patch_hunk.Patch.mine_start;
    mine_len = patch_hunk.Patch.mine_len;
    their_start = patch_hunk.Patch.their_start;
    their_len = patch_hunk.Patch.their_len;
    lines = process_changes [] patch_hunk.Patch.lines;
  }

let word_to_ui word attr = W.string ~attr (word ^ " ")

let render_diff_line mine_num their_num attr diff_type diff =
  let format_line_number =
    match diff_type with
    | `Added -> W.string ~attr (Printf.sprintf "   %2d + " (their_num + 1))
    | `Deleted -> W.string ~attr (Printf.sprintf "%2d    - " (mine_num + 1))
    | `Equal ->
        W.string ~attr:Notty.A.empty
          (Printf.sprintf "%2d %2d   " (mine_num + 1) (their_num + 1))
  in
  Ui.hcat
    [
      format_line_number;
      Ui.hcat
        (List.map
           (function
             | WDeleted words when diff_type = `Deleted ->
                 word_to_ui (words_to_string words) attr
             | WAdded words when diff_type = `Added ->
                 word_to_ui (words_to_string words) attr
             | WEqual words -> word_to_ui (words_to_string words) Notty.A.empty
             | _ -> Ui.empty)
           diff);
    ]

let render_line_diff mine_num their_num = function
  | Common text ->
      ( mine_num + 1,
        their_num + 1,
        render_diff_line mine_num their_num Notty.A.empty `Equal
          [ WEqual (string_to_words text) ] )
  | Deleted text ->
      ( mine_num + 1,
        their_num,
        render_diff_line mine_num their_num
          Notty.A.(fg red)
          `Deleted
          [ WDeleted (string_to_words text) ] )
  | Added text ->
      ( mine_num,
        their_num + 1,
        render_diff_line mine_num their_num
          Notty.A.(fg green)
          `Added
          [ WAdded (string_to_words text) ] )
  | Modified diff ->
      ( mine_num + 1,
        their_num + 1,
        Ui.vcat
          [
            render_diff_line mine_num their_num
              Notty.A.(fg red)
              `Deleted
              (List.filter
                 (function WDeleted _ | WEqual _ -> true | _ -> false)
                 diff);
            render_diff_line mine_num their_num
              Notty.A.(fg green)
              `Added
              (List.filter
                 (function WAdded _ | WEqual _ -> true | _ -> false)
                 diff);
          ] )

let render_hunk (hunk : hunk) : Nottui.ui =
  let lines_ui =
    let rec process_lines mine_num their_num acc = function
      | [] -> List.rev acc
      | line :: rest ->
          let new_mine, new_their, ui =
            render_line_diff mine_num their_num line
          in
          process_lines new_mine new_their (ui :: acc) rest
    in
    process_lines hunk.mine_start hunk.their_start [] hunk.lines
  in
  Ui.vcat lines_ui
