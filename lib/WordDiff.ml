open Nottui
module W = Nottui_widgets

type word = Unchanged of string | Changed of string
type line_content = word list

let string_to_words s = Array.of_list (String.split_on_char ' ' s)
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

let diff_words (s1 : string) (s2 : string) : line_content * line_content =
  let words1 = Array.to_list (string_to_words s1) in
  let words2 = Array.to_list (string_to_words s2) in
  let common = lcs words1 words2 in

  let rec construct_diff w1 w2 lcs acc_mine acc_their =
    match (w1, w2, lcs) with
    | [], [], [] -> (List.rev acc_mine, List.rev acc_their)
    | x :: xs, y :: ys, z :: zs -> (
        match (x = z, y = z) with
        | true, true ->
            construct_diff xs ys zs (Unchanged x :: acc_mine)
              (Unchanged y :: acc_their)
        | false, true ->
            construct_diff xs (y :: ys) (z :: zs) (Changed x :: acc_mine)
              acc_their
        | true, false ->
            construct_diff (x :: xs) ys (z :: zs) acc_mine
              (Changed y :: acc_their)
        | false, false ->
            construct_diff xs ys (z :: zs) (Changed x :: acc_mine)
              (Changed y :: acc_their))
    | x :: xs, [], lcs ->
        construct_diff xs [] lcs (Changed x :: acc_mine) acc_their
    | [], y :: ys, lcs ->
        construct_diff [] ys lcs acc_mine (Changed y :: acc_their)
    | x :: xs, y :: ys, [] ->
        construct_diff xs ys [] (Changed x :: acc_mine) (Changed y :: acc_their)
    | [], [], _ :: _ -> assert false
    (* Since lcs is the longest common subsequence, this case cannot happen *)
  in

  construct_diff words1 words2 common [] []

let compute (block : string Block.t) : line_content Block.t =
  match block with
  | Block.Common line -> Block.Common [ Unchanged line ]
  | Block.Changed { mine; their; order } ->
      let mine_str = String.concat " " mine in
      let their_str = String.concat " " their in
      let mine_words, their_words = diff_words mine_str their_str in
      Block.Changed { mine = [ mine_words ]; their = [ their_words ]; order }

let word_to_ui word attr rendering_mode diff_type =
  match rendering_mode with
  | Types.Color -> W.string ~attr (word ^ " ")
  | Types.TextMarkers -> (
      match diff_type with
      | `Equal -> W.string (word ^ " ")
      | `Deleted -> W.string ("<-" ^ word ^ "/-> ")
      | `Added -> W.string ("<+" ^ word ^ "/+> "))

let render_diff_line mine_num their_num attr diff_type words rendering_mode =
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
             | Changed word when diff_type = `Deleted || diff_type = `Added ->
                 word_to_ui word attr rendering_mode diff_type
             | Unchanged word ->
                 word_to_ui word Notty.A.empty rendering_mode `Equal
             | _ -> Ui.empty)
           words);
    ]

let render_hunk_lines (hunk_lines : line_content Patch.line list)
    (rendering_mode : Types.rendering_mode) : Nottui.ui =
  let rec process_lines mine_num their_num acc = function
    | [] -> List.rev acc
    | line :: rest ->
        let new_mine, new_their, ui =
          match line with
          | `Common words ->
              ( mine_num + 1,
                their_num + 1,
                render_diff_line mine_num their_num Notty.A.empty `Equal words
                  rendering_mode )
          | `Mine words ->
              ( mine_num + 1,
                their_num,
                render_diff_line mine_num their_num
                  Notty.A.(fg red)
                  `Deleted words rendering_mode )
          | `Their words ->
              ( mine_num,
                their_num + 1,
                render_diff_line mine_num their_num
                  Notty.A.(fg green)
                  `Added words rendering_mode )
        in
        process_lines new_mine new_their (ui :: acc) rest
  in
  let lines_ui = process_lines 0 0 [] hunk_lines in
  Ui.vcat lines_ui

let render_hunk (hunk : string Patch.hunk)
    (rendering_mode : Types.rendering_mode) : Nottui.ui =
  let lines_ui =
    let rec process_lines mine_num their_num acc = function
      | [] -> List.rev acc
      | line :: rest ->
          let new_mine, new_their, ui =
            match line with
            | `Common s ->
                ( mine_num + 1,
                  their_num + 1,
                  match rendering_mode with
                  | Types.Color ->
                      W.string ~attr:Notty.A.(fg lightblue) ("  " ^ s)
                  | Types.TextMarkers -> W.string ("   " ^ s) )
            | `Mine s ->
                ( mine_num + 1,
                  their_num,
                  match rendering_mode with
                  | Types.Color -> W.string ~attr:Notty.A.(fg red) ("- " ^ s)
                  | Types.TextMarkers -> W.string ("<- " ^ s ^ " /->") )
            | `Their s ->
                ( mine_num,
                  their_num + 1,
                  match rendering_mode with
                  | Types.Color -> W.string ~attr:Notty.A.(fg green) ("+ " ^ s)
                  | Types.TextMarkers -> W.string ("<+ " ^ s ^ " /+>") )
          in
          process_lines new_mine new_their (ui :: acc) rest
    in
    process_lines hunk.Patch.mine_start hunk.Patch.their_start []
      hunk.Patch.lines
  in
  Ui.vcat lines_ui
