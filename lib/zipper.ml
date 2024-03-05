(** This file contains the zipper implementation *)

type 'a zipper = {
  before: 'a list;  
  current: 'a option;
  after: 'a list;
}

(** a function to initialize a zipper given a list *)
let zipper_of_list (lst : 'a list) = {
  before = [];
  current = (
    match lst with
    | [] -> 
      None 
    | x :: _ ->
      Some x)
  ;
  after = (
    match lst with
    | [] ->
      []
    | _ :: xs ->
      xs
  );
}

(** a function to test the zipper_of_list function *)
let test_zipper_of_list candidate =
  let open List in
  let lst = [1; 2; 3; 4; 5] in
  let z = candidate lst in
  let test_init = (z.before = []) && (z.current = Some 1) && (z.after = [2; 3; 4; 5]) in
  test_init

(** a function to access the next element in a zipper *)
let next_zipper z = match z.after with
  | [] ->
      z 
  | x :: xs ->
    {
      before = (
        match z.current with
        | None ->
          z.before
        | Some c ->
          c :: z.before
      );
      current = Some x;
      after = xs
    }

(** a function to test the next_zipper function *)
let test_next_zipper candidate =
  let initial_zipper = {before = []; current = Some 1; after = [2; 3; 4; 5]} in
  let next_z = candidate initial_zipper in
  let test_next = (next_z.before = [1]) && (next_z.current = Some 2) && (next_z.after = [3; 4; 5]) in
  test_next


(** a function to access the previous element in a zipper *)
let prev_zipper z = match z.before with
  | [] ->
      z 
  | x :: xs ->
      {
      before = xs;
      current = Some x;
      after = (
        match z.current with
        | None ->
          z.after
        | Some c ->
          c :: z.after
      )
    }

(** a function to test the prev_zipper function *)
let test_prev_zipper candidate =
  let initial_zipper = {before = [1]; current = Some 2; after = [3; 4; 5]} in
  let prev_z = candidate initial_zipper in
  let test_prev = (prev_z.before = []) && (prev_z.current = Some 1) && (prev_z.after = [2; 3; 4; 5]) in
  test_prev

(** a function to run the tests *)

let () =
  let () = assert (test_zipper_of_list zipper_of_list) in
  let () = assert (test_next_zipper next_zipper) in
  let () = assert (test_prev_zipper prev_zipper) in
  Printf.printf "All tests passed!\n"
