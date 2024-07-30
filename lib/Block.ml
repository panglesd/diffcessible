type origin = Mine | Their

type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : origin }

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
    let make_block ~adds ~dels =
      if adds = [] && dels = [] then []
      else
        let order = if adds <> [] then Mine else Their in
        [ Changed { mine = adds; their = dels; order } ]
    in
    let make_common c = [ Common c ] in
    let finish_block acc adds dels rest next_state =
      let new_acc = make_block ~adds ~dels @ acc in
      match rest with
      | [] -> List.rev new_acc
      | `Common x :: rest' -> next_state (make_common x @ new_acc) rest'
      | _ -> next_state new_acc rest
    in
    let rec process acc = function
      | [] -> List.rev acc
      | `Mine _ :: _ as lines ->
          let adds, rest = collect_consecutive_added lines in
          (match rest with
           | `Their _ :: _ ->
               let dels, rest' = collect_consecutive_removed rest in
               finish_block acc adds dels rest' process
           | _ -> finish_block acc adds [] rest process)
      | `Their _ :: _ as lines ->
          let dels, rest = collect_consecutive_removed lines in
          (match rest with
           | `Mine _ :: _ ->
               let adds, rest' = collect_consecutive_added rest in
               finish_block acc adds dels rest' process
           | _ -> finish_block acc [] dels rest process)
      | `Common x :: rest -> process (make_common x @ acc) rest
    in
    process [] hunk_lines
let to_hunk (blocks : 'a t list) : 'a Patch.line list =
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
  lines;
