open Nottui
module W = Nottui_widgets

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

type line_change =
  | CommonWord of string
  | AddedWord of string
  | DeletedWord of string
  | ModifiedDiff of word_diff list

type hunk = {
  mine_start : int;
  mine_len : int;
  their_start : int;
  their_len : int;
  lines : line_change list;
}

let string_to_words s = Array.of_list (String.split_on_char ' ' s)
let words_to_string words = String.concat " " (Array.to_list words)
let longest xs ys = if List.length xs > List.length ys then xs else ys

let lcs xs' ys' =
  let xs = Array.of_list xs' and ys = Array.of_list ys' in
  let n = Array.length xs and m = Array.length ys in
  let a = Array.make_matrix (n + 1) (m + 1) [] in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      a.(i).(j) <-
        (if xs.(i) = ys.(j) then xs.(i) :: a.(i + 1).(j + 1)
         else longest a.(i).(j + 1) a.(i + 1).(j))
    done
  done;
  a.(0).(0)

let diff_words s1 s2 =
  let words1 = Array.to_list (string_to_words s1) in
  let words2 = Array.to_list (string_to_words s2) in
  let common = lcs words1 words2 in
  let rec construct_diff w1 w2 lcs acc =
    match (w1, w2, lcs) with
    | [], [], [] -> List.rev acc
    | x :: xs, y :: ys, z :: zs -> (
        match (x = z, y = z) with
        | true, true -> construct_diff xs ys zs (WEqual [| x |] :: acc)
        | false, true ->
            construct_diff xs (y :: ys) (z :: zs) (WDeleted [| x |] :: acc)
        | true, false ->
            construct_diff (x :: xs) ys (z :: zs) (WAdded [| y |] :: acc)
        | false, false ->
            construct_diff xs ys (z :: zs)
              (WDeleted [| x |] :: WAdded [| y |] :: acc))
    | x :: xs, [], lcs -> construct_diff xs [] lcs (WDeleted [| x |] :: acc)
    | [], y :: ys, lcs -> construct_diff [] ys lcs (WAdded [| y |] :: acc)
    | x :: xs, y :: ys, [] ->
        construct_diff xs ys [] (WDeleted [| x |] :: WAdded [| y |] :: acc)
    | [], [], _ ->
        failwith
          "This should not happen, lcs should be the longest common \
           subsequence of the two lists."
  in

  construct_diff words1 words2 common []

let apply_word_diff s1 s2 = diff_words s1 s2

let compute (patch_hunk : Patch.hunk) : hunk =
  let rec process_changes acc = function
    | [] -> List.rev acc
    | `Mine m :: `Their t :: rest ->
        let diff = apply_word_diff m t in
        process_changes (ModifiedDiff diff :: acc) rest
    | `Their t :: rest -> process_changes (AddedWord t :: acc) rest
    | `Mine m :: rest -> process_changes (DeletedWord m :: acc) rest
    | `Common c :: rest -> process_changes (CommonWord c :: acc) rest
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
  | CommonWord text ->
      ( mine_num + 1,
        their_num + 1,
        render_diff_line mine_num their_num Notty.A.empty `Equal
          [ WEqual (string_to_words text) ] )
  | DeletedWord text ->
      ( mine_num + 1,
        their_num,
        render_diff_line mine_num their_num
          Notty.A.(fg red)
          `Deleted
          [ WDeleted (string_to_words text) ] )
  | AddedWord text ->
      ( mine_num,
        their_num + 1,
        render_diff_line mine_num their_num
          Notty.A.(fg green)
          `Added
          [ WAdded (string_to_words text) ] )
  | ModifiedDiff diff ->
      ( mine_num + 1,
        their_num + 1,
        Ui.vcat
          [
            render_diff_line mine_num their_num Notty.A.(fg red) `Deleted diff;
            render_diff_line mine_num their_num Notty.A.(fg green) `Added diff;
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
