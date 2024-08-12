open Diffcessible

let test_lcs_1 = ([ "a"; "b"; "c" ], [ "a"; "b"; "c" ], [ "a"; "b"; "c" ])
let test_lcs_2 = ([ "a"; "b"; "c" ], [ "d"; "e"; "f" ], [])

let test_lcs_3 =
  ( [ "a"; "b"; "c"; "d"; "f"; "g"; "h"; "j"; "q"; "z" ],
    [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "i"; "j"; "k"; "r"; "x"; "y"; "z" ],
    [ "a"; "b"; "c"; "d"; "f"; "g"; "j"; "z" ] )

let test_lcs_4 = ([], [], []) (* Empty lists *)

let test_lcs_5 =
  ([ "a"; "a"; "b"; "b"; "c" ], [ "a"; "b"; "c"; "c" ], [ "a"; "b"; "c" ])

(* Repeating elements *)
let test_lcs_6 = ([ "a"; "b"; "c" ], [ "d"; "e"; "f" ], [])

(* All different, same length *)
let test_lcs_7 = ([], [ "a"; "b"; "c" ], []) (* One empty, one non-empty *)
let test_lcs_8 = ([ "a"; "b"; "c" ], [], []) (* One non-empty, one empty *)

let test_diff_1 =
  ( "abc def",
    "abc ghi",
    ( [ WordDiff.Unchanged "abc"; WordDiff.Changed "def" ],
      [ WordDiff.Unchanged "abc"; WordDiff.Changed "ghi" ] ) )

let test_diff_2 =
  ( "",
    "abc def",
    ([ WordDiff.Changed "" ], [ WordDiff.Changed "abc"; WordDiff.Changed "def" ])
  )

let test_diff_3 =
  ( "abc def",
    "",
    ([ WordDiff.Changed "abc"; WordDiff.Changed "def" ], [ WordDiff.Changed "" ])
  )

let test_diff_4 =
  ( "abc def ghi",
    "abc def jkl",
    ( [
        WordDiff.Unchanged "abc";
        WordDiff.Unchanged "def";
        WordDiff.Changed "ghi";
      ],
      [
        WordDiff.Unchanged "abc";
        WordDiff.Unchanged "def";
        WordDiff.Changed "jkl";
      ] ) )

let test_diff_5 =
  ( "  abc  def  ",
    " abc def ",
    ( [
        WordDiff.Unchanged "";
        WordDiff.Changed "";
        WordDiff.Unchanged "abc";
        WordDiff.Changed "";
        WordDiff.Unchanged "def";
        WordDiff.Unchanged "";
        WordDiff.Changed "";
      ],
      [
        WordDiff.Unchanged "";
        WordDiff.Unchanged "abc";
        WordDiff.Unchanged "def";
        WordDiff.Unchanged "";
      ] ) )

let string_of_word = function
  | WordDiff.Unchanged s -> Printf.sprintf "Unchanged %S" s
  | WordDiff.Changed s -> Printf.sprintf "Changed %S" s

let string_of_line_content lc =
  "[" ^ String.concat "; " (List.map string_of_word lc) ^ "]"

let string_of_diff_result (mine, their) =
  Printf.sprintf "(%s, %s)"
    (string_of_line_content mine)
    (string_of_line_content their)

let assert_with_message condition message =
  if not condition then failwith message

let test_lcs () =
  let test_case (input1, input2, expected) =
    let result = WordDiff.lcs input1 input2 in
    assert_with_message (result = expected)
      (Printf.sprintf
         "LCS failed\nInput1: %s\nInput2: %s\nExpected: %s\nGot: %s"
         (String.concat " " input1) (String.concat " " input2)
         (String.concat " " expected)
         (String.concat " " result))
  in
  List.iter test_case
    [
      test_lcs_1;
      test_lcs_2;
      test_lcs_3;
      test_lcs_4;
      test_lcs_5;
      test_lcs_6;
      test_lcs_7;
      test_lcs_8;
    ]

let test_diff_words () =
  let test_case (input1, input2, expected) =
    let result = WordDiff.diff_words input1 input2 in
    assert_with_message (result = expected)
      (Printf.sprintf
         "diff_words failed\nInput1: %S\nInput2: %S\nExpected: %s\nGot: %s"
         input1 input2
         (string_of_diff_result expected)
         (string_of_diff_result result))
  in
  List.iter test_case
    [ test_diff_1; test_diff_2; test_diff_3; test_diff_4; test_diff_5 ]

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
  run_test "test_lcs" test_lcs;
  run_test "test_diff_words" test_diff_words

let () = run_tests ()
