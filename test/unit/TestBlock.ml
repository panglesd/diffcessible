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

type origin = Mine | Their | Equal

type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : origin }

let example_blocks =
  [
    Changed { mine = [ "A" ]; their = [ "B" ]; order = Mine };
    Changed { mine = [ "C" ]; their = [ "D" ]; order = Mine };
  ]

let find_first_change (hunk : string Patch.hunk) : origin =
  let rec aux lines =
    match lines with
    | `Common _ :: rest -> aux rest
    | `Mine _ :: _ -> Mine
    | `Their _ :: _ -> Their
    | [] -> Equal
  in
  aux hunk.lines

type collection_phase = First | Second

let of_hunk (hunk : string Patch.hunk) : string t list =
  let first_change = find_first_change hunk in
  let rec aux blocks mine their lines current_origin phase =
    match lines with
    | [] ->
        if mine <> [] || their <> [] then
          Changed
            {
              mine = List.rev mine;
              their = List.rev their;
              order = current_origin;
            }
          :: blocks
        else blocks
    | `Common x :: rest ->
        let new_blocks =
          if mine <> [] || their <> [] then
            Changed
              {
                mine = List.rev mine;
                their = List.rev their;
                order = current_origin;
              }
            :: blocks
          else blocks
        in
        aux (Common x :: new_blocks) [] [] rest first_change First
    | `Mine x :: rest -> (
        match (phase, current_origin) with
        | First, _ -> aux blocks (x :: mine) their rest Mine Second
        | Second, Mine -> aux blocks (x :: mine) their rest Mine Second
        | Second, Their | Second, Equal ->
            let new_blocks =
              Changed
                {
                  mine = List.rev mine;
                  their = List.rev their;
                  order = current_origin;
                }
              :: blocks
            in
            aux new_blocks [ x ] [] rest Mine First)
    | `Their x :: rest -> (
        match (phase, current_origin) with
        | First, _ -> aux blocks mine (x :: their) rest Their Second
        | Second, Their -> aux blocks mine (x :: their) rest Their Second
        | Second, Mine | Second, Equal ->
            let new_blocks =
              Changed
                {
                  mine = List.rev mine;
                  their = List.rev their;
                  order = current_origin;
                }
              :: blocks
            in
            aux new_blocks [] [ x ] rest Their First)
  in
  List.rev (aux [] [] [] hunk.lines first_change First)

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
            (match order with
            | Mine -> "Mine"
            | Their -> "Their"
            | Equal -> "Equal"))
    result;
  Printf.printf "Expected result:\n";
  List.iter
    (function
      | Common x -> Printf.printf "  Common: %s\n" x
      | Changed { mine; their; order } ->
          Printf.printf "  Changed: mine=[%s], their=[%s], order=%s\n"
            (String.concat ";" mine) (String.concat ";" their)
            (match order with
            | Mine -> "Mine"
            | Their -> "Their"
            | Equal -> "Equal"))
    example_blocks;
  assert (result = example_blocks);
  Printf.printf "test_of_hunk passed!\n"

let () = test_of_hunk ()
