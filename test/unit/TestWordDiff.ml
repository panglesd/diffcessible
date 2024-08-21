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

let test_edit_distance_1 =
  ( [| 'k'; 'i'; 't'; 't'; 'e'; 'n' |],
    [| 's'; 'i'; 't'; 't'; 'i'; 'n'; 'g' |],
    3 )

let test_edit_distance_2 =
  ( [| 'S'; 'u'; 'n'; 'd'; 'a'; 'y' |],
    [| 'S'; 'a'; 't'; 'u'; 'r'; 'd'; 'a'; 'y' |],
    3 )

let test_edit_distance_3 = ([| 'a'; 'b'; 'c' |], [| 'a'; 'b'; 'c' |], 0)
let test_edit_distance_4 = ([| 'a'; 'b'; 'c' |], [| 'd'; 'e'; 'f' |], 3)
let test_edit_distance_5 = ([||], [| 'a'; 'b'; 'c' |], 3)
let test_edit_distance_6 = ([| 'a'; 'b'; 'c' |], [||], 3)
let test_edit_distance_7 = ([||], [||], 0) (* Both empty *)
let test_edit_distance_8 = ([| 'a' |], [| 'b' |], 1)
let test_edit_distance_9 = ([| 'a'; 'a'; 'a' |], [| 'a'; 'a'; 'a'; 'a' |], 1)
let test_edit_distance_10 = ([| 'a'; 'a'; 'a'; 'a' |], [| 'a'; 'a'; 'a' |], 1)
let test_edit_distance_11 = ([| '1'; '2'; '3' |], [| '2'; '3'; '4' |], 2)

let test_edit_distance_12 =
  ([| 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |], [| 'a'; 'z'; 'c'; 'd'; 'e'; 'f' |], 1)

let test_edit_distance () =
  let test_case (input1, input2, expected) =
    let result = WordDiff.edit_distance Char.equal input1 input2 in
    assert_with_message (result = expected)
      (Printf.sprintf
         "edit_distance failed\nInput1: %s\nInput2: %s\nExpected: %d\nGot: %d"
         (String.of_seq (Array.to_seq input1))
         (String.of_seq (Array.to_seq input2))
         expected result)
  in
  List.iter test_case
    [
      test_edit_distance_1;
      test_edit_distance_2;
      test_edit_distance_3;
      test_edit_distance_4;
      test_edit_distance_5;
      test_edit_distance_6;
      test_edit_distance_7;
      test_edit_distance_8;
      test_edit_distance_9;
      test_edit_distance_10;
      test_edit_distance_11;
      test_edit_distance_12;
    ]

let test_pair_lines_1 =
  ( [| "foo"; "bar baz" |],
    [| "bar bat" |],
    [ (Some "foo", None); (Some "bar baz", Some "bar bat") ] )

let test_pair_lines_2 =
  ( [| "line1"; "line2"; "line3 old" |],
    [| "line1 new"; "line2"; "line3 updated" |],
    [
      (Some "line1", Some "line1 new");
      (Some "line2", Some "line2");
      (Some "line3 old", Some "line3 updated");
    ] )

let test_pair_lines_3 =
  ( [| "a"; "b"; "c" |],
    [| "x"; "y"; "z" |],
    [ (Some "a", Some "x"); (Some "b", Some "y"); (Some "c", Some "z") ] )

let test_pair_lines_4 =
  ( [| "same"; "line"; "different" |],
    [| "same"; "line"; "changed" |],
    [
      (Some "same", Some "same");
      (Some "line", Some "line");
      (Some "different", Some "changed");
    ] )

let test_pair_lines_5 =
  ( [| "extra"; "lines"; "here" |],
    [| "lines" |],
    [ (Some "extra", None); (Some "lines", Some "lines"); (Some "here", None) ]
  )

let test_pair_lines_6 =
  ( [| "A"; "B"; "C" |],
    [| "B"; "D" |],
    [
      (Some "A", None); (Some "B", Some "B"); (Some "C", None); (None, Some "D");
    ] )

let test_pair_lines_7 =
  ( [| "This"; "is"; "a"; "test" |],
    [| "This"; "was"; "a"; "test" |],
    [
      (Some "This", Some "This");
      (Some "is", Some "was");
      (Some "a", Some "a");
      (Some "test", Some "test");
    ] )

let test_pair_lines_8 =
  ( [| "One"; "Two"; "Three" |],
    [| "1"; "2"; "3" |],
    [ (Some "One", Some "1"); (Some "Two", Some "2"); (Some "Three", Some "3") ]
  )

let test_pair_lines_9 =
  ( [| "Hello"; "World" |],
    [| "Hello"; "there"; "World" |],
    [
      (Some "Hello", Some "Hello");
      (None, Some "there");
      (Some "World", Some "World");
    ] )

let test_pair_lines_10 =
  ( [| "First"; "Second"; "Third"; "Fourth" |],
    [| "1st"; "2nd"; "3rd" |],
    [
      (Some "First", Some "1st");
      (Some "Second", Some "2nd");
      (Some "Third", Some "3rd");
      (Some "Fourth", None);
    ] )

let test_pair_lines_11 =
  ( [| "first_line"; "second_line" |],
    [| "second_lines"; "first_lines" |],
    [
      (None, Some "second_lines");
      (Some "first_line", Some "first_lines");
      (Some "second_line", None);
    ] )

let test_pair_lines_12 =
  ( [| "first_line"; "second_line" |],
    [| "second_lines"; "first_lines" |],
    [
      (Some "first_line", None);
      (Some "second_line", Some "second_lines");
      (None, Some "first_lines");
    ] )

let test_pair_lines_13 =
  ( [| "first_line"; "second_line" |],
    [| "second_line"; "first_line" |],
    [
      (None, Some "second_line");
      (Some "first_line", Some "first_line");
      (Some "second_line", None);
    ] )

let test_pair_lines_14 =
  ( [| "A"; "B"; "C" |],
    [| "A"; "C"; "B" |],
    [
      (Some "A", Some "A");
      (Some "B", None);
      (Some "C", Some "C");
      (None, Some "B");
    ] )

let test_pair_lines_15 =
  ( [| "X"; "Y"; "Z" |],
    [| "Z"; "X"; "Y" |],
    [
      (Some "X", None);
      (Some "Y", None);
      (Some "Z", Some "Z");
      (None, Some "X");
      (None, Some "Y");
    ] )

let test_pair_lines_16 =
  ( [| "1"; "2"; "3"; "4" |],
    [| "4"; "2"; "1"; "3" |],
    [
      (Some "1", None);
      (Some "2", Some "2");
      (Some "3", None);
      (Some "4", Some "4");
      (None, Some "1");
      (None, Some "3");
    ] )

let test_pair_lines_17 =
  ( [| "alpha"; "beta"; "gamma"; "delta" |],
    [| "beta"; "delta"; "omega"; "alpha" |],
    [
      (Some "alpha", None);
      (Some "beta", Some "beta");
      (Some "gamma", None);
      (Some "delta", Some "delta");
      (None, Some "omega");
      (None, Some "alpha");
    ] )

(*
  The pairing algorithm is exactly the same as the algorithm we use to find word-level diffs. 
  However instead of exact equality, we use approximate equality, which is to be close
  enough to the edit distance. 

  We need to specify a threshold for the edit distance to be X much, where X is a function of the input.
  So the point of this number is to turn the approximate equality function into a bool function.

  Create a general diff module for this, and WordDiff calls this function from this general module.
*)

(*
  What breaking the order means when pairing lines:
    Pairing in increasing order
    Ordering:
      A C
      B D
      
      NOT GOOD:
      A D
      B C

    This will make it hard to display to the user what the diff comparison was like
 *)

let string_of_option = function
  | Some s -> Printf.sprintf "Some %S" s
  | None -> "None"

let string_of_pair (a, b) =
  Printf.sprintf "(%s, %s)" (string_of_option a) (string_of_option b)

let string_of_paired_lines pairs =
  "[" ^ String.concat "; " (List.map string_of_pair pairs) ^ "]"

let test_pair_lines () =
  let test_case name (input1, input2, expected) =
    let result = WordDiff.pair_lines 3 input1 input2 in
    assert_with_message (result = expected)
      (Printf.sprintf "%s failed\nInput1: %s\nInput2: %s\nExpected: %s\nGot: %s"
         name
         (String.concat " " (Array.to_list input1))
         (String.concat " " (Array.to_list input2))
         (string_of_paired_lines expected)
         (string_of_paired_lines result))
  in
  List.iter
    (fun (name, test) -> test_case name test)
    [
      ("test_pair_lines_1", test_pair_lines_1);
      ("test_pair_lines_2", test_pair_lines_2);
      ("test_pair_lines_3", test_pair_lines_3);
      ("test_pair_lines_4", test_pair_lines_4);
      ("test_pair_lines_5", test_pair_lines_5);
      ("test_pair_lines_6", test_pair_lines_6);
      ("test_pair_lines_7", test_pair_lines_7);
      ("test_pair_lines_8", test_pair_lines_8);
      ("test_pair_lines_9", test_pair_lines_9);
      ("test_pair_lines_10", test_pair_lines_10);
      ("test_pair_lines_11", test_pair_lines_11);
      ("test_pair_lines_12", test_pair_lines_12);
      ("test_pair_lines_13", test_pair_lines_13);
      ("test_pair_lines_14", test_pair_lines_14);
      ("test_pair_lines_15", test_pair_lines_15);
      ("test_pair_lines_16", test_pair_lines_16);
      ("test_pair_lines_17", test_pair_lines_17);
    ]

(* Update the run_tests function to include all tests *)
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
  run_test "test_diff_words" test_diff_words;
  run_test "test_edit_distance" test_edit_distance;
  run_test "test_pair_lines" test_pair_lines

let () = run_tests ()
