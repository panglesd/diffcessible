type origin = Mine | Their

type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : origin }

let of_hunk (hunk : 'a Patch.hunk) : 'a t list =
  let make_block ~adds ~dels =
    if adds = [] && dels = [] then []
    else
      let order = if adds <> [] then Mine else Their in
      [ Changed { mine = List.rev adds; their = List.rev dels; order } ]
  in
  let make_common c = [ Common c ] in
  let finish_block acc adds dels rest next_state =
    let new_acc = make_block ~adds ~dels @ acc in
    match rest with
    | [] -> List.rev new_acc
    | `Common x :: rest' -> next_state (make_common x @ new_acc) rest'
    | _ -> next_state new_acc rest
  in
  let rec start acc = function
    | [] -> List.rev acc
    | `Mine x :: rest -> collect_mine acc [ x ] rest
    | `Their x :: rest -> collect_their acc [ x ] rest
    | `Common x :: rest -> start (make_common x @ acc) rest
  and collect_mine acc adds = function
    | `Mine x :: rest -> collect_mine acc (x :: adds) rest
    | `Their x :: rest -> collect_their_after_mine acc adds [ x ] rest
    | rest -> finish_block acc adds [] rest start
  and collect_their acc dels = function
    | `Their x :: rest -> collect_their acc (x :: dels) rest
    | `Mine x :: rest -> collect_mine_after_their acc [ x ] dels rest
    | rest -> finish_block acc [] dels rest start
  and collect_their_after_mine acc adds dels = function
    | `Their x :: rest -> collect_their_after_mine acc adds (x :: dels) rest
    | rest -> finish_block acc adds dels rest (fun acc -> collect_mine acc [])
  and collect_mine_after_their acc adds dels = function
    | `Mine x :: rest -> collect_mine_after_their acc (x :: adds) dels rest
    | rest -> finish_block acc adds dels rest (fun acc -> collect_their acc [])
  in
  start [] hunk.lines

let to_hunk (blocks : 'a t list) : 'a Patch.hunk =
  let lines =
    List.concat_map
      (function
        | Common x -> [ `Common x ]
        | Changed { mine; their; order } ->
            let mine_lines = List.map (fun x -> `Mine x) mine in
            let their_lines = List.map (fun x -> `Their x) their in
            if order = Mine then mine_lines @ their_lines
            else their_lines @ mine_lines)
      blocks
  in
  let mine_count =
    List.length (List.filter (function `Mine _ -> true | _ -> false) lines)
  in
  let their_count =
    List.length (List.filter (function `Their _ -> true | _ -> false) lines)
  in
  {
    Patch.mine_start = 0;
    mine_len = mine_count;
    their_start = 0;
    their_len = their_count;
    lines;
  }
