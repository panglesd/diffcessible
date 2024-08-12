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

  let rec edit_distance_helper i j =
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
                   (edit_distance_helper (i - 1) j + 1)
                   (edit_distance_helper i (j - 1) + 1))
                (edit_distance_helper (i - 1) (j - 1) + cost_to_drop_both)
            in
            Hashtbl.add memo (i, j) result;
            result)
  in
  edit_distance_helper (Array.length s) (Array.length t)

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
      let mine_str = String.concat " " mine in
      let their_str = String.concat " " their in
      let mine_words, their_words = diff_words mine_str their_str in
      Block.Changed { mine = [ mine_words ]; their = [ their_words ]; order }
