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

let is_approximately_equal s1 s2 =
  let compare_char c1 c2 = c1 = c2 in
  let s1_array = Array.of_seq (String.to_seq s1) in
  let s2_array = Array.of_seq (String.to_seq s2) in
  let threshold = max 3 (max (String.length s1) (String.length s2) / 2) in
  edit_distance compare_char s1_array s2_array <= threshold

let lacs words1 words2 =
  let m = List.length words1 and n = List.length words2 in
  let dp = Array.make_matrix (m + 1) (n + 1) [] in
  for i = 1 to m do
    for j = 1 to n do
      let c1 = List.nth words1 (i - 1) in
      let c2 = List.nth words2 (j - 1) in
      if is_approximately_equal c1 c2 then
        dp.(i).(j) <- (c1, c2) :: dp.(i - 1).(j - 1)
      else if List.length dp.(i - 1).(j) > List.length dp.(i).(j - 1) then
        dp.(i).(j) <- dp.(i - 1).(j)
      else dp.(i).(j) <- dp.(i).(j - 1)
    done
  done;
  List.rev dp.(m).(n)

(*
The LCAS does is try to find the two subsequences, one from each sequence, such that the elements of the two subsequences are pair-wise
almost equal. So there is no notion of the best one, just the longest subsequence which is equivalent to finding the biggest pairing.
This was my previous implementation (the greedy algorithm) of finding the pairing.
*)

let pair_lines (s1 : string array) (s2 : string array) :
    (string option * string option) list =
  let words1 = Array.to_list s1 in
  let words2 = Array.to_list s2 in
  let common = lacs words1 words2 in

  let rec construct_pairs w1 w2 lacs acc =
    match (w1, w2, lacs) with
    | [], [], [] -> List.rev acc
    | x :: xs, y :: ys, ((z1, z2) as z) :: zs -> (
        match (String.equal x z1, String.equal y z2) with
        | true, true -> construct_pairs xs ys zs ((Some x, Some y) :: acc)
        | false, _ ->
            construct_pairs xs (y :: ys) (z :: zs) ((Some x, None) :: acc)
        | _, false ->
            construct_pairs (x :: xs) ys (z :: zs) ((None, Some y) :: acc))
    | x :: xs, [], lacs -> construct_pairs xs [] lacs ((Some x, None) :: acc)
    | [], y :: ys, lacs -> construct_pairs [] ys lacs ((None, Some y) :: acc)
    | x :: xs, ys, [] -> construct_pairs xs ys [] ((Some x, None) :: acc)
    | [], [], _ :: _ -> assert false
    (* Since lacs is the longest almost common subsequence, this case cannot happen *)
  in

  construct_pairs words1 words2 common []

let compute (block : string Block.t) : line_content Block.t list =
  match block with
  | Block.Common line -> [ Block.Common [ Unchanged line ] ]
  | Block.Changed { mine; their; order } ->
      let mine_array = Array.of_list mine in
      let their_array = Array.of_list their in
      let paired_lines = pair_lines mine_array their_array in

      let diff_words line1 line2 =
        match (line1, line2) with
        | Some l1, Some l2 ->
            let l1, l2 = diff_words l1 l2 in
            Block.Changed { mine = [ l1 ]; their = [ l2 ]; order }
        | Some l1, None ->
            Block.Changed { mine = [ [ Changed l1 ] ]; their = []; order }
        | None, Some l2 ->
            Block.Changed { mine = []; their = [ [ Changed l2 ] ]; order }
        | None, None -> assert false
      in

      List.map (fun (m, t) -> diff_words m t) paired_lines

(* Block.Changed *)
(*   { mine = List.rev mine_content; their = List.rev their_content; order } *)
