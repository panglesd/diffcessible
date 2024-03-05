open Zipper

(** a function to test the zipper_of_list function *)
let test_zipper_of_list candidate =
  let lst = [1; 2; 3; 4; 5] in
  let z = candidate lst in
  let test_init = match z with
    | None -> false
    | Some z -> (z.before = []) && (z.focus = 1) && (z.after = [2; 3; 4; 5]) in
  test_init

(** a function to test the get_zipper_of_list *)

let test_get_zipper_of_list candidate =
  let initial_zipper = zipper_of_list [1; 2; 3; 4; 5] in
  let z = candidate initial_zipper in
  let test_init = match z with
    | z -> (z.before = []) && (z.focus = 1) && (z.after = [2; 3; 4; 5]) in
  test_init

(** a function to test the next_zipper function *)
let test_next_zipper candidate =
  let initial_zipper = {before = []; focus = 1; after = [2; 3; 4; 5]} in
  let next_z = candidate initial_zipper in
  let test_next = (next_z.before = [1]) && (next_z.focus = 2) && (next_z.after = [3; 4; 5]) in
  test_next

(** a function to test the prev_zipper function *)
let test_prev_zipper candidate =
  let initial_zipper = {before = [1]; focus = 2; after = [3; 4; 5]} in
  let prev_z = candidate initial_zipper in
  let test_prev = (prev_z.before = []) && (prev_z.focus = 1) && (prev_z.after = [2; 3; 4; 5]) in
  test_prev


let () =
  assert (test_zipper_of_list zipper_of_list);
  assert (test_get_zipper_of_list get_zipper_of_list);
  assert (test_next_zipper next_zipper);
  assert (test_prev_zipper prev_zipper);
