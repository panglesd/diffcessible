type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : Types.block_origin }

let rec first_change_order (hunk_lines : 'a Patch.line list) :
    Types.block_origin =
  match hunk_lines with
  | [] -> None
  | `Common _ :: rest -> first_change_order rest
  | `Mine _ :: _ -> Mine
  | `Their _ :: _ -> Their

let of_hunk (hunk_lines : 'a Patch.line list) : 'a t list =
  let collect_consecutive_added lines =
    let rec aux acc lines =
      match lines with
      | `Mine x :: rest -> aux (x :: acc) rest
      | rest -> (List.rev acc, rest)
    in
    aux [] lines
  in
  let collect_consecutive_removed lines =
    let rec aux acc lines =
      match lines with
      | `Their x :: rest -> aux (x :: acc) rest
      | rest -> (List.rev acc, rest)
    in
    aux [] lines
  in

  let make_block ~adds ~dels remaining_lines =
    let order = first_change_order remaining_lines in
    Changed { mine = adds; their = dels; order }
  in
  let rec process acc = function
    | [] -> List.rev acc
    | `Mine x :: rest as lines ->
        let adds, rest' = collect_consecutive_added (`Mine x :: rest) in
        let dels, rest'' = collect_consecutive_removed rest' in
        process (make_block ~adds ~dels lines :: acc) rest''
    | `Their x :: rest as lines ->
        let dels, rest' = collect_consecutive_removed (`Their x :: rest) in
        let adds, rest'' = collect_consecutive_added rest' in
        process (make_block ~adds ~dels lines :: acc) rest''
    | `Common x :: rest -> process (Common x :: acc) rest
  in
  process [] hunk_lines

let to_hunk (blocks : 'a t list) : 'a Patch.line list =
  List.concat_map
    (function
      | Common x -> [ `Common x ]
      | Changed { mine; their; order } ->
          let mine_lines = List.map (fun x -> `Mine x) mine in
          let their_lines = List.map (fun x -> `Their x) their in
          if order = Types.Mine then mine_lines @ their_lines
          else their_lines @ mine_lines)
    blocks
