type 'a t = Common of 'a | Changed of { mine : 'a list; their : 'a list }

let of_hunk (hunk : 'a Patch.hunk) : 'a t list =
  let rec process_hunk acc current_mine current_their = function
    | [] ->
        if current_mine <> [] || current_their <> [] then
          Changed
            { mine = List.rev current_mine; their = List.rev current_their }
          :: acc
        else acc
    | `Common line :: rest ->
        let acc' =
          if current_mine <> [] || current_their <> [] then
            Changed
              { mine = List.rev current_mine; their = List.rev current_their }
            :: acc
          else acc
        in
        process_hunk (Common line :: acc') [] [] rest
    | `Mine line :: rest ->
        process_hunk acc (line :: current_mine) current_their rest
    | `Their line :: rest ->
        process_hunk acc current_mine (line :: current_their) rest
  in
  List.rev (process_hunk [] [] [] hunk.lines)

let to_hunk (blocks : 'a t list) : 'a Patch.hunk =
  let lines, mine_len, their_len =
    List.fold_left
      (fun (lines, mine_len, their_len) block ->
        match block with
        | Common line -> (`Common line :: lines, mine_len + 1, their_len + 1)
        | Changed { mine; their } ->
            let mine_lines = List.map (fun line -> `Mine line) mine in
            let their_lines = List.map (fun line -> `Their line) their in
            ( mine_lines @ their_lines @ lines,
              mine_len + List.length mine,
              their_len + List.length their ))
      ([], 0, 0) blocks
  in
  {
    mine_start = 0;
    mine_len;
    their_start = 0;
    their_len;
    lines = List.rev lines;
  }
