type 'a t = Common of 'a | Changed of { mine : 'a list; their : 'a list }

let of_hunk (hunk : 'a Patch.hunk) : 'a t list =
  let rec aux acc current_mine current_their lines =
    match lines with
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
        aux (Common line :: acc') [] [] rest
    | `Mine line :: rest -> aux acc (line :: current_mine) current_their rest
    | `Their line :: rest -> aux acc current_mine (line :: current_their) rest
  in
  List.rev (aux [] [] [] hunk.lines)

let to_hunk (blocks : 'a t list) : 'a Patch.hunk =
  let rec aux lines mine_len their_len blocks =
    match blocks with
    | [] -> (lines, mine_len, their_len)
    | Common line :: rest ->
        aux (`Common line :: lines) (mine_len + 1) (their_len + 1) rest
    | Changed { mine; their } :: rest ->
        let mine_lines = List.map (fun line -> `Mine line) mine in
        let their_lines = List.map (fun line -> `Their line) their in
        aux
          (mine_lines @ their_lines @ lines)
          (mine_len + List.length mine)
          (their_len + List.length their)
          rest
  in
  let lines, mine_len, their_len = aux [] 0 0 blocks in
  {
    mine_start = 0;
    mine_len;
    their_start = 0;
    their_len;
    lines = List.rev lines;
  }
