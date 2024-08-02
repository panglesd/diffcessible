type block_origin = Mine | Their | None
type 'a block_content = Entry of 'a | Newline

type 'a t =
  | Common of 'a block_content
  | Changed of {
      mine : 'a block_content list;
      their : 'a block_content list;
      order : block_origin;
    }

let rec first_change_order (hunk_lines : 'a Patch.line list) : block_origin =
  match hunk_lines with
  | [] -> None
  | `Common _ :: rest -> first_change_order rest
  | `Mine _ :: _ -> Mine
  | `Their _ :: _ -> Their

let of_hunk (hunk_lines : 'a Patch.line list) : 'a t list =
  let collect_consecutive_added lines =
    let rec aux acc lines =
      match lines with
      | `Mine x :: rest -> aux (Newline :: Entry x :: acc) rest
      | rest -> (acc, rest)
    in
    aux [] lines
  in
  let collect_consecutive_removed lines =
    let rec aux acc lines =
      match lines with
      | `Their x :: rest -> aux (Newline :: Entry x :: acc) rest
      | rest -> (acc, rest)
    in
    aux [] lines
  in

  let make_block ~adds ~dels remaining_lines =
    let order = first_change_order remaining_lines in
    Changed { mine = List.rev adds; their = List.rev dels; order }
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
    | `Common x :: rest -> process (Common (Entry x) :: acc) rest
  in
  process [] hunk_lines

let to_hunk (blocks : 'a t list) : 'a Patch.line list =
  let lines =
    List.concat_map
      (function
        | Common x -> (
            match x with
            | Entry content -> [ `Common content ]
            | Newline -> [] (* Skip Newline for Common blocks *))
        | Changed { mine; their; order } ->
            let mine_lines =
              List.filter_map
                (function
                  | Entry content -> Some (`Mine content) | Newline -> None)
                mine
            in
            let their_lines =
              List.filter_map
                (function
                  | Entry content -> Some (`Their content) | Newline -> None)
                their
            in
            if order = Mine then mine_lines @ their_lines
            else their_lines @ mine_lines)
      blocks
  in
  lines
