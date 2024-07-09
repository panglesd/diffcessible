type diff_line = Common of string | Mine of string | Their of string

let word_diff (old_str : string) (new_str : string) : string =
  let old_words : string list = String.split_on_char ' ' old_str in
  let new_words : string list = String.split_on_char ' ' new_str in

  let find_middle_snake (start_old : int) (end_old : int) (start_new : int)
      (end_new : int) : (int * int) option =
    let m : int = end_old - start_old in
    let n : int = end_new - start_new in
    let delta : int = m - n in
    let max_d = (m + n + 1) / 2 in
    let forward : int array = Array.make ((2 * max_d) + 1) 0 in
    let backward : int array = Array.make ((2 * max_d) + 1) 0 in

    let rec loop (d : int) : (int * int) option =
      if d > max_d then None
      else
        let found_snake : (int * int) option ref = ref None in
        for k = -d to d do
          let k_val : int =
            if
              k = -d
              || (k <> d && forward.(k - 1 + max_d) < forward.(k + 1 + max_d))
            then forward.(k + 1 + max_d)
            else forward.(k - 1 + max_d) + 1
          in
          let x : int ref = ref k_val in
          let y : int ref = ref (k_val - k) in
          while
            !x < m && !y < n
            && String.equal
                 (List.nth old_words (start_old + !x))
                 (List.nth new_words (start_new + !y))
          do
            incr x;
            incr y
          done;
          forward.(k + max_d) <- !x;

          let c : int = k - delta in
          if c >= -d && c <= d then (
            let c_val : int =
              if
                c = -d
                || c <> d
                   && backward.(c - 1 + max_d) > backward.(c + 1 + max_d)
              then backward.(c + 1 + max_d)
              else backward.(c - 1 + max_d) - 1
            in
            let x : int ref = ref c_val in
            let y : int ref = ref (c_val - c) in
            while
              !x > 0 && !y > 0
              && String.equal
                   (List.nth old_words (end_old - !x))
                   (List.nth new_words (end_new - !y))
            do
              decr x;
              decr y
            done;
            backward.(c + max_d) <- !x;

            if forward.(k + max_d) >= m - backward.(c + max_d) then
              let start_snake : int = forward.(k + max_d) in
              let end_snake : int = n - backward.(c + max_d) in
              if start_snake + end_snake >= m then
                found_snake :=
                  Some (start_old + start_snake, start_new + end_snake))
        done;

        match !found_snake with Some _ -> !found_snake | None -> loop (d + 1)
    in
    loop 0
  in

  let rec diff_recursive (start_old : int) (end_old : int) (start_new : int)
      (end_new : int) : diff_line list =
    if start_old >= end_old && start_new >= end_new then []
    else if start_old >= end_old then
      List.init (end_new - start_new) (fun i ->
          Their (List.nth new_words (start_new + i)))
    else if start_new >= end_new then
      List.init (end_old - start_old) (fun i ->
          Mine (List.nth old_words (start_old + i)))
    else
      match find_middle_snake start_old end_old start_new end_new with
      | Some (split_old, split_new) ->
          let left_diff : diff_line list =
            diff_recursive start_old split_old start_new split_new
          in
          let right_diff : diff_line list =
            diff_recursive split_old end_old split_new end_new
          in
          left_diff @ right_diff
      | None ->
          let old_sublist : string list =
            List.init (end_old - start_old) (fun i ->
                List.nth old_words (start_old + i))
          in
          let new_sublist : string list =
            List.init (end_new - start_new) (fun i ->
                List.nth new_words (start_new + i))
          in
          let rec merge_diffs (acc : diff_line list) (old_words : string list)
              (new_words : string list) : diff_line list =
            match (old_words, new_words) with
            | [], [] -> acc
            | old_word :: old_rest, [] ->
                merge_diffs (Mine old_word :: acc) old_rest []
            | [], new_word :: new_rest ->
                merge_diffs (Their new_word :: acc) [] new_rest
            | old_word :: old_rest, new_word :: new_rest ->
                if String.equal old_word new_word then
                  merge_diffs (Common old_word :: acc) old_rest new_rest
                else
                  merge_diffs
                    (Mine old_word :: Their new_word :: acc)
                    old_rest new_rest
          in
          List.rev (merge_diffs [] old_sublist new_sublist)
  in

  let diff_lines : diff_line list =
    diff_recursive 0 (List.length old_words) 0 (List.length new_words)
  in

  let rec build_diff_string (acc : string list) (diff_lines : diff_line list) :
      string =
    match diff_lines with
    | [] -> String.concat "\n" (List.rev acc)
    | Common word :: rest -> build_diff_string (word :: acc) rest
    | Mine word :: Their new_word :: rest ->
        let line : string = Printf.sprintf "- %s\n+ %s" word new_word in
        build_diff_string (line :: acc) rest
    | Mine word :: rest ->
        let line : string = Printf.sprintf "- %s" word in
        build_diff_string (line :: acc) rest
    | Their word :: rest ->
        let line : string = Printf.sprintf "+ %s" word in
        build_diff_string (line :: acc) rest
  in

  build_diff_string [] diff_lines

(* Example usage *)
let old_str : string = "Foo bar baz"
let new_str : string = "Foo baz bar"
let diff : string = word_diff old_str new_str
let () = print_endline diff

