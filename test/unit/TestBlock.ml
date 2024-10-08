open Diffcessible

let example_hunk1 : string Patch.line list =
  [ `Mine "A"; `Their "B"; `Mine "C"; `Their "D" ]

let example_blocks1 : string Block.t list =
  [
    Block.Changed { mine = [ "A" ]; their = [ "B" ]; order = Mine };
    Block.Changed { mine = [ "C" ]; their = [ "D" ]; order = Mine };
  ]

let empty_hunk : string Patch.line list = []
let empty_blocks : string Block.t list = []

let common_only_hunk : string Patch.line list =
  [ `Common "A"; `Common "B"; `Common "C" ]

let common_only_blocks : string Block.t list =
  [ Block.Common "A"; Block.Common "B"; Block.Common "C" ]

let mine_only_hunk : string Patch.line list =
  [ `Mine "A"; `Mine "B"; `Mine "C" ]

let mine_only_blocks : string Block.t list =
  [ Block.Changed { mine = [ "A"; "B"; "C" ]; their = []; order = Mine } ]

let their_only_hunk : string Patch.line list =
  [ `Their "X"; `Their "Y"; `Their "Z" ]

let their_only_blocks : string Block.t list =
  [ Block.Changed { mine = []; their = [ "X"; "Y"; "Z" ]; order = Their } ]

let complex_hunk : string Patch.line list =
  [
    `Common "A";
    `Mine "B";
    `Their "C";
    `Mine "D";
    `Their "E";
    `Common "F";
    `Mine "G";
    `Their "H";
    `Their "I";
  ]

let complex_blocks : string Block.t list =
  [
    Block.Common "A";
    Block.Changed { mine = [ "B" ]; their = [ "C" ]; order = Mine };
    Block.Changed { mine = [ "D" ]; their = [ "E" ]; order = Mine };
    Block.Common "F";
    Block.Changed { mine = [ "G" ]; their = [ "H"; "I" ]; order = Mine };
  ]

let alternating_add_remove_hunk1 : string Patch.line list =
  [ `Mine "A"; `Their "B"; `Mine "C" ]

let alternating_add_remove_blocks1 : string Block.t list =
  [
    Block.Changed { mine = [ "A" ]; their = [ "B" ]; order = Mine };
    Block.Changed { mine = [ "C" ]; their = []; order = Mine };
  ]

let alternating_remove_add_hunk1 : string Patch.line list =
  [ `Their "X"; `Mine "Y"; `Their "Z" ]

let alternating_remove_add_blocks1 : string Block.t list =
  [
    Block.Changed { mine = [ "Y" ]; their = [ "X" ]; order = Their };
    Block.Changed { mine = []; their = [ "Z" ]; order = Their };
  ]

let complex_alternating_hunk : string Patch.line list =
  [
    `Mine "A";
    `Their "B";
    `Mine "C";
    `Their "D";
    `Mine "E";
    `Their "F";
    `Mine "G";
    `Their "H";
    `Common "I";
    `Their "J";
    `Mine "K";
    `Their "L";
    `Mine "M";
  ]

let complex_alternating_blocks : string Block.t list =
  [
    Block.Changed { mine = [ "A" ]; their = [ "B" ]; order = Mine };
    Block.Changed { mine = [ "C" ]; their = [ "D" ]; order = Mine };
    Block.Changed { mine = [ "E" ]; their = [ "F" ]; order = Mine };
    Block.Changed { mine = [ "G" ]; their = [ "H" ]; order = Mine };
    Block.Common "I";
    Block.Changed { mine = [ "K" ]; their = [ "J" ]; order = Their };
    Block.Changed { mine = [ "M" ]; their = [ "L" ]; order = Their };
  ]

let multiple_consecutive_changes_hunk : string Patch.line list =
  [
    `Mine "A";
    `Mine "B";
    `Their "C";
    `Their "D";
    `Mine "E";
    `Mine "F";
    `Their "G";
    `Their "H";
    `Common "I";
    `Their "J";
    `Their "K";
    `Mine "L";
    `Mine "M";
  ]

let multiple_consecutive_changes_blocks : string Block.t list =
  [
    Block.Changed { mine = [ "A"; "B" ]; their = [ "C"; "D" ]; order = Mine };
    Block.Changed { mine = [ "E"; "F" ]; their = [ "G"; "H" ]; order = Mine };
    Block.Common "I";
    Block.Changed { mine = [ "L"; "M" ]; their = [ "J"; "K" ]; order = Their };
  ]

let string_of_block = function
  | Block.Common x -> Printf.sprintf "Common %S" x
  | Block.Changed { mine; their; order } ->
      Printf.sprintf "Changed { mine = [%s]; their = [%s]; order = %s }"
        (String.concat "; " (List.map (Printf.sprintf "%S") mine))
        (String.concat "; " (List.map (Printf.sprintf "%S") their))
        (match order with Mine -> "Mine" | Their -> "Their" | None -> "None")

let string_of_blocks blocks =
  "[" ^ String.concat "; " (List.map string_of_block blocks) ^ "]"

let string_of_hunk_line = function
  | `Common s -> Printf.sprintf "`Common %S" s
  | `Mine s -> Printf.sprintf "`Mine %S" s
  | `Their s -> Printf.sprintf "`Their %S" s

let string_of_hunk hunk =
  "[" ^ String.concat "; " (List.map string_of_hunk_line hunk) ^ "]"

let assert_with_message condition message =
  if not condition then failwith message

let test_of_hunk () =
  let test_case name hunk expected =
    let result = Block.of_hunk hunk in
    assert_with_message (result = expected)
      (Printf.sprintf "of_hunk failed for %s\nExpected: %s\nGot: %s" name
         (string_of_blocks expected)
         (string_of_blocks result))
  in
  test_case "example_hunk1" example_hunk1 example_blocks1;
  test_case "empty_hunk" empty_hunk empty_blocks;
  test_case "common_only_hunk" common_only_hunk common_only_blocks;
  test_case "mine_only_hunk" mine_only_hunk mine_only_blocks;
  test_case "their_only_hunk" their_only_hunk their_only_blocks;
  test_case "complex_hunk" complex_hunk complex_blocks;
  test_case "alternating_add_remove_hunk1" alternating_add_remove_hunk1
    alternating_add_remove_blocks1;
  test_case "alternating_remove_add_hunk1" alternating_remove_add_hunk1
    alternating_remove_add_blocks1;
  test_case "complex_alternating_hunk" complex_alternating_hunk
    complex_alternating_blocks;
  test_case "multiple_consecutive_changes_hunk"
    multiple_consecutive_changes_hunk multiple_consecutive_changes_blocks

let test_to_hunk () =
  let test_case name blocks expected =
    let result = Block.to_hunk blocks in
    assert_with_message (result = expected)
      (Printf.sprintf "to_hunk failed for %s\nExpected: %s\nGot: %s" name
         (string_of_hunk expected) (string_of_hunk result))
  in
  test_case "example_blocks1" example_blocks1 example_hunk1;
  test_case "empty_blocks" empty_blocks empty_hunk;
  test_case "common_only_blocks" common_only_blocks common_only_hunk;
  test_case "mine_only_blocks" mine_only_blocks mine_only_hunk;
  test_case "their_only_blocks" their_only_blocks their_only_hunk;
  test_case "complex_blocks" complex_blocks complex_hunk;
  test_case "alternating_add_remove_blocks1" alternating_add_remove_blocks1
    alternating_add_remove_hunk1;
  test_case "alternating_remove_add_blocks1" alternating_remove_add_blocks1
    alternating_remove_add_hunk1;
  test_case "complex_alternating_blocks" complex_alternating_blocks
    complex_alternating_hunk;
  test_case "multiple_consecutive_changes_blocks"
    multiple_consecutive_changes_blocks multiple_consecutive_changes_hunk

let test_roundtrip () =
  let test_roundtrip_for name hunk =
    let roundtrip = hunk |> Block.of_hunk |> Block.to_hunk in
    assert_with_message (roundtrip = hunk)
      (Printf.sprintf "Roundtrip failed for %s\nOriginal: %s\nRoundtrip: %s"
         name (string_of_hunk hunk) (string_of_hunk roundtrip))
  in
  test_roundtrip_for "example_hunk1" example_hunk1;
  test_roundtrip_for "empty_hunk" empty_hunk;
  test_roundtrip_for "common_only_hunk" common_only_hunk;
  test_roundtrip_for "mine_only_hunk" mine_only_hunk;
  test_roundtrip_for "their_only_hunk" their_only_hunk;
  test_roundtrip_for "complex_hunk" complex_hunk;
  test_roundtrip_for "alternating_add_remove_hunk1" alternating_add_remove_hunk1;
  test_roundtrip_for "alternating_remove_add_hunk1" alternating_remove_add_hunk1;
  test_roundtrip_for "complex_alternating_hunk" complex_alternating_hunk;
  test_roundtrip_for "multiple_consecutive_changes_hunk"
    multiple_consecutive_changes_hunk

let run_tests () =
  let run_test name f =
    try
      Printf.printf "Running %s...\n" name;
      f ();
      Printf.printf "%s passed.\n\n" name
    with
    | Failure msg -> Printf.printf "%s failed:\n%s\n\n" name msg
    | exn ->
        Printf.printf "%s failed with unexpected exception:\n%s\n\n" name
          (Printexc.to_string exn)
  in
  run_test "TestBlock::test_of_hunk" test_of_hunk;
  run_test "TestBlock::test_to_hunk" test_to_hunk;
  run_test "TestBlock::test_roundtrip" test_roundtrip

let () = run_tests ()
