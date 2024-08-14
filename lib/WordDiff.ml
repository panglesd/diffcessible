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
      (* Helper function to convert a string to an array of words *)
      let string_to_word_array s =
        s |> String.split_on_char ' ' |> Array.of_list
      in

      (* Use Levenshtein distance to find the best pairing of lines *)
      let pair_lines lines1 lines2 =
        let distances =
          Array.make_matrix (Array.length lines1) (Array.length lines2) 0
        in
        for i = 0 to Array.length lines1 - 1 do
          for j = 0 to Array.length lines2 - 1 do
            distances.(i).(j) <-
              edit_distance ( = )
                (string_to_word_array lines1.(i))
                (string_to_word_array lines2.(j))
          done
        done;

        (* Use a greedy approach to pair lines based on minimum distance *)
        let paired = ref [] in
        let used1 = Array.make (Array.length lines1) false in
        let used2 = Array.make (Array.length lines2) false in

        for _ = 1 to min (Array.length lines1) (Array.length lines2) do
          let min_dist = ref max_int in
          let min_i = ref (-1) in
          let min_j = ref (-1) in

          for i = 0 to Array.length lines1 - 1 do
            for j = 0 to Array.length lines2 - 1 do
              if
                (not used1.(i))
                && (not used2.(j))
                && distances.(i).(j) < !min_dist
              then (
                min_dist := distances.(i).(j);
                min_i := i;
                min_j := j)
            done
          done;

          if !min_i <> -1 && !min_j <> -1 then (
            paired := (!min_i, !min_j) :: !paired;
            used1.(!min_i) <- true;
            used2.(!min_j) <- true)
        done;

        (* Add unpaired lines *)
        let final_pairs = ref !paired in
        for i = 0 to Array.length lines1 - 1 do
          if not used1.(i) then final_pairs := (i, -1) :: !final_pairs
        done;
        for j = 0 to Array.length lines2 - 1 do
          if not used2.(j) then final_pairs := (-1, j) :: !final_pairs
        done;

        List.sort compare !final_pairs
      in

      let mine_array = Array.of_list mine in
      let their_array = Array.of_list their in
      let pairs = pair_lines mine_array their_array in

      let result_mine = ref [] in
      let result_their = ref [] in

      (* Helper function to trim leading/trailing whitespace and collapse multiple spaces *)
      let normalize_whitespace s =
        let s = String.trim s in
        let buf = Buffer.create (String.length s) in
        let space_seen = ref false in
        String.iter
          (fun c ->
            match c with
            | ' ' | '\t' | '\n' | '\r' ->
                if not !space_seen then (
                  Buffer.add_char buf ' ';
                  space_seen := true)
            | _ ->
                Buffer.add_char buf c;
                space_seen := false)
          s;
        Buffer.contents buf
      in

      List.iter
        (fun (i, j) ->
          match (i, j) with
          | -1, j ->
              let their_content = normalize_whitespace their_array.(j) in
              if their_content <> "" then
                result_their :=
                  (diff_words "" their_content |> snd) :: !result_their
          | i, -1 ->
              let mine_content = normalize_whitespace mine_array.(i) in
              if mine_content <> "" then
                result_mine :=
                  (diff_words mine_content "" |> fst) :: !result_mine
          | i, j ->
              let mine_content = normalize_whitespace mine_array.(i) in
              let their_content = normalize_whitespace their_array.(j) in
              if mine_content <> "" || their_content <> "" then (
                let mine_diff, their_diff =
                  diff_words mine_content their_content
                in
                result_mine := mine_diff :: !result_mine;
                result_their := their_diff :: !result_their))
        pairs;

      (* Remove any empty lines at the beginning and end of the results *)
      let trim_empty_lines lines =
        let is_empty line =
          List.for_all (function Unchanged "" -> true | _ -> false) line
        in
        let rec trim_start = function
          | [] -> []
          | hd :: tl when is_empty hd -> trim_start tl
          | lines -> lines
        in
        let rec trim_end = function
          | [] -> []
          | hd :: tl ->
              let trimmed_tail = trim_end tl in
              if trimmed_tail = [] && is_empty hd then []
              else hd :: trimmed_tail
        in
        lines |> trim_start |> trim_end
      in

      let trimmed_mine = trim_empty_lines (List.rev !result_mine) in
      let trimmed_their = trim_empty_lines (List.rev !result_their) in

      Block.Changed { mine = trimmed_mine; their = trimmed_their; order }
