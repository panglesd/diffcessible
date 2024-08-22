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

let edit_distance (type a) (compare : a -> a -> bool) (s : a array)
    (t : a array) : int =
  let memo = Hashtbl.create ((Array.length s + 1) * (Array.length t + 1)) in

  let rec edit_distance_aux i j =
    match (i, j) with
    | 0, x | x, 0 -> x
    | i, j -> (
        match Hashtbl.find_opt memo (i, j) with
        | Some result -> result
        | None ->
            let result =
              let cost_to_drop_both =
                if compare s.(i - 1) t.(j - 1) then 0 else 1
              in
              min
                (min
                   (edit_distance_aux (i - 1) j + 1)
                   (edit_distance_aux i (j - 1) + 1))
                (edit_distance_aux (i - 1) (j - 1) + cost_to_drop_both)
            in
            Hashtbl.add memo (i, j) result;
            result)
  in
  edit_distance_aux (Array.length s) (Array.length t)

let pair_lines (lines1 : string array) (lines2 : string array) :
    string option list * string option list =
  let calculate_threshold (line1 : string) (line2 : string) : int =
    let len1 = String.length line1 in
    let len2 = String.length line2 in
    let max_len = max len1 len2 in
    max 15 max_len
  in

  let is_approximately_equal (line1 : string) (line2 : string) : bool =
    let threshold = calculate_threshold line1 line2 in
    let distance =
      edit_distance
        (fun c1 c2 -> c1 = c2)
        (Array.of_seq (String.to_seq line1))
        (Array.of_seq (String.to_seq line2))
    in
    distance <= threshold
  in

  let lines1 = Array.to_list lines1 in
  let lines2 = Array.to_list lines2 in
  let common = lcs lines1 lines2 in

  let rec pair_lines_aux l1 l2 lcs acc_mine acc_their =
    match (l1, l2, lcs) with
    | [], [], [] -> (List.rev acc_mine, List.rev acc_their)
    | x :: xs, y :: ys, z :: zs -> (
        match (is_approximately_equal x z, is_approximately_equal y z) with
        | true, true ->
            pair_lines_aux xs ys zs (Some x :: acc_mine) (Some y :: acc_their)
        | false, true ->
            pair_lines_aux xs (y :: ys) (z :: zs) (Some x :: acc_mine)
              (None :: acc_their)
        | true, false ->
            pair_lines_aux (x :: xs) ys (z :: zs) (None :: acc_mine)
              (Some y :: acc_their)
        | false, false ->
            pair_lines_aux xs ys (z :: zs) (Some x :: acc_mine)
              (Some y :: acc_their))
    | x :: xs, [], lcs ->
        pair_lines_aux xs [] lcs (Some x :: acc_mine) (None :: acc_their)
    | [], y :: ys, lcs ->
        pair_lines_aux [] ys lcs (None :: acc_mine) (Some y :: acc_their)
    | x :: xs, y :: ys, [] ->
        pair_lines_aux xs ys [] (Some x :: acc_mine) (Some y :: acc_their)
    | [], [], _ :: _ -> assert false
    (* Since lcs is the longest common subsequence, this case cannot happen *)
  in
  pair_lines_aux lines1 lines2 common [] []

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
      let mine_array = Array.of_list mine in
      let their_array = Array.of_list their in
      let paired_mine, paired_their = pair_lines mine_array their_array in

      let diff_words line1 line2 =
        match (line1, line2) with
        | Some l1, Some l2 -> diff_words l1 l2
        | Some l, None | None, Some l ->
            let words = String.split_on_char ' ' l in
            (List.map (fun w -> Changed w) words, [])
        | None, None -> ([], [])
      in

      let rec process_pairs mine_acc their_acc = function
        | [], [] -> (List.rev mine_acc, List.rev their_acc)
        | m :: ms, t :: ts ->
            let mine_content, their_content = diff_words m t in
            process_pairs (mine_content :: mine_acc)
              (their_content :: their_acc)
              (ms, ts)
        | m :: ms, [] ->
            let mine_content, _ = diff_words m None in
            process_pairs (mine_content :: mine_acc) their_acc (ms, [])
        | [], t :: ts ->
            let _, their_content = diff_words None t in
            process_pairs mine_acc (their_content :: their_acc) ([], ts)
      in

      let mine_content, their_content =
        process_pairs [] [] (paired_mine, paired_their)
      in

      Block.Changed { mine = mine_content; their = their_content; order }
