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

(* Test cases *)

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
    ([ Unchanged "abc"; Changed "def" ], [ Unchanged "abc"; Changed "ghi" ]) )

let test_diff_2 =
  ("", "abc def", ([ Changed "" ], [ Changed "abc"; Changed "def" ]))

let test_diff_3 =
  ("abc def", "", ([ Changed "abc"; Changed "def" ], [ Changed "" ]))

let test_diff_4 =
  ( "abc def ghi",
    "abc def jkl",
    ( [ Unchanged "abc"; Unchanged "def"; Changed "ghi" ],
      [ Unchanged "abc"; Unchanged "def"; Changed "jkl" ] ) )

let test_diff_5 =
  ( "  abc  def  ",
    " abc def ",
    ( [
        Unchanged "";
        Changed "";
        Unchanged "abc";
        Changed "";
        Unchanged "def";
        Unchanged "";
        Changed "";
      ],
      [ Unchanged ""; Unchanged "abc"; Unchanged "def"; Unchanged "" ] ) )

let string_of_word = function
  | Unchanged s -> Printf.sprintf "Unchanged %S" s
  | Changed s -> Printf.sprintf "Changed %S" s

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
    let result = lcs input1 input2 in
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
    let result = diff_words input1 input2 in
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
