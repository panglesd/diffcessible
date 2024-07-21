open Nottui
module W = Nottui_widgets

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

type line_diff =
  | CommonDiff of string
  | DeletedDiff of word_diff list
  | AddedDiff of word_diff list
  | ModifiedDiff of word_diff list * word_diff list

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
  | CommonDiff text ->
      ( mine_num + 1,
        their_num + 1,
        render_diff_line mine_num their_num Notty.A.empty `Equal
          [ WEqual (string_to_words text) ] )
  | DeletedDiff diff ->
      ( mine_num + 1,
        their_num,
        render_diff_line mine_num their_num Notty.A.(fg red) `Deleted diff )
  | AddedDiff diff ->
      ( mine_num,
        their_num + 1,
        render_diff_line mine_num their_num Notty.A.(fg green) `Added diff )
  | ModifiedDiff (mine_diff, their_diff) ->
      ( mine_num + 1,
        their_num + 1,
        Ui.vcat
          [
            render_diff_line mine_num their_num
              Notty.A.(fg red)
              `Deleted mine_diff;
            render_diff_line mine_num their_num
              Notty.A.(fg green)
              `Added their_diff;
          ] )
