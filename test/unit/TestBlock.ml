let example_hunk : string Patch.hunk =
  {
    mine_start = 0;
    mine_len = 2;
    their_start = 0;
    their_len = 2;
    lines = [ `Mine "A"; `Their "B"; `Mine "C"; `Their "D" ];
  }
(* +A *)
(* -B *)
(* +C *)
(* -D *)

type origin = Mine | Their

type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : origin }

let example_blocks =
  [
    Changed { mine = [ "A" ]; their = [ "B" ]; order = Mine };
    Changed { mine = [ "C" ]; their = [ "D" ]; order = Mine };
  ]

(* let find_first_change (hunk : string Patch.hunk) : origin = *)
(*   let rec aux lines = *)
(*     match lines with *)
(*     | `Common _ :: rest -> aux rest *)
(*     | `Mine _ :: _ -> Mine *)
(*     | `Their _ :: _ -> Their *)
(*     | [] -> Equal *)
(*   in *)
(*   aux hunk.lines *)

let of_hunk (hunk : 'a Patch.hunk) : 'a t list =
  let make_block ~adds ~dels =
    if adds = [] && dels = [] then []
    else
      let order = if adds <> [] then Mine else Their in
      [ Changed { mine = adds; their = dels; order } ]
  in
  let make_common c = [ Common c ] in

  let rec start acc = function
    | [] -> List.rev acc
    | `Mine x :: rest -> collect_mine acc [ x ] rest
    | `Their x :: rest -> collect_their acc [ x ] rest
    | `Common x :: rest -> start (make_common x @ acc) rest
  and collect_mine acc adds = function
    | [] -> List.rev (make_block ~adds ~dels:[] @ acc)
    | `Mine x :: rest -> collect_mine acc (x :: adds) rest
    | `Their x :: rest ->
        collect_their_after_mine acc (List.rev adds) [ x ] rest
    | `Common x :: rest ->
        start (make_block ~adds ~dels:[] @ make_common x @ acc) rest
  and collect_their acc dels = function
    | [] -> List.rev (make_block ~adds:[] ~dels @ acc)
    | `Mine x :: rest -> collect_mine_after_their acc [ x ] (List.rev dels) rest
    | `Their x :: rest -> collect_their acc (x :: dels) rest
    | `Common x :: rest ->
        start (make_block ~adds:[] ~dels @ make_common x @ acc) rest
  and collect_their_after_mine acc adds dels = function
    | [] -> List.rev (make_block ~adds ~dels @ acc)
    | `Mine x :: rest -> collect_mine (make_block ~adds ~dels @ acc) [ x ] rest
    | `Their x :: rest -> collect_their_after_mine acc adds (x :: dels) rest
    | `Common x :: rest ->
        start (make_block ~adds ~dels @ make_common x @ acc) rest
  and collect_mine_after_their acc adds dels = function
    | [] -> List.rev (make_block ~adds ~dels @ acc)
    | `Mine x :: rest -> collect_mine_after_their acc (x :: adds) dels rest
    | `Their x :: rest ->
        collect_their (make_block ~adds ~dels @ acc) [ x ] rest
    | `Common x :: rest ->
        start (make_block ~adds ~dels @ make_common x @ acc) rest
  in

  start [] hunk.lines

let test_of_hunk () =
  Printf.printf "Starting test_of_hunk\n";
  let result = of_hunk example_hunk in
  Printf.printf "of_hunk result:\n";
  List.iter
    (function
      | Common x -> Printf.printf "  Common: %s\n" x
      | Changed { mine; their; order } ->
          Printf.printf "  Changed: mine=[%s], their=[%s], order=%s\n"
            (String.concat ";" mine) (String.concat ";" their)
            (match order with Mine -> "Mine" | Their -> "Their"))
    result;
  Printf.printf "Expected result:\n";
  List.iter
    (function
      | Common x -> Printf.printf "  Common: %s\n" x
      | Changed { mine; their; order } ->
          Printf.printf "  Changed: mine=[%s], their=[%s], order=%s\n"
            (String.concat ";" mine) (String.concat ";" their)
            (match order with Mine -> "Mine" | Their -> "Their"))
    example_blocks;
  assert (result = example_blocks);
  Printf.printf "test_of_hunk passed!\n"

let () = test_of_hunk ()
