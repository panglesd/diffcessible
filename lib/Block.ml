type 'a t = Common of 'a | Changed of { mine : 'a list; their : 'a list }

(* Helper function implementations *)
let rec group_lines acc current_mine current_their = function
  | [] ->
      if current_mine <> [] || current_their <> [] then
        Changed { mine = current_mine; their = current_their } :: acc
      else acc
  | `Common line :: rest ->
      let acc' =
        if current_mine <> [] || current_their <> [] then
          Changed { mine = current_mine; their = current_their } :: acc
        else acc
      in
      group_lines (Common line :: acc') [] [] rest
  | `Mine line :: rest ->
      group_lines acc (line :: current_mine) current_their rest
  | `Their line :: rest ->
      group_lines acc current_mine (line :: current_their) rest

let rec process_blocks lines mine_len their_len = function
  | [] -> (lines, mine_len, their_len)
  | Common line :: rest ->
      process_blocks (`Common line :: lines) (mine_len + 1) (their_len + 1) rest
  | Changed { mine; their } :: rest ->
      let mine_lines = List.rev_map (fun line -> `Mine line) mine in
      let their_lines = List.rev_map (fun line -> `Their line) their in
      process_blocks
        (List.rev_append mine_lines (List.rev_append their_lines lines))
        (mine_len + List.length mine)
        (their_len + List.length their)
        rest

(* End of helper function implementations *)

(* Main block function implementations *)
let of_hunk (_hunk : 'a Patch.hunk) : 'a t list =
  List.rev (group_lines [] [] [] _hunk.lines)

let to_hunk (_blocks : 'a t list) : 'a Patch.hunk =
  let lines, mine_len, their_len = process_blocks [] 0 0 _blocks in
  {
    mine_start = 0;
    mine_len;
    their_start = 0;
    their_len;
    lines = List.rev lines;
  }

(* End of main block function implementations *)
