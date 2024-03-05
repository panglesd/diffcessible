
(** a function to test the zipper_of_list function *)
let test_zipper_of_list candidate =
  let b0 = 
    let lst = [1; 2; 3; 4; 5] in
    let z = candidate lst in
    let test_b0 = Diffcessible.Zipper.get_focus z = 1 
                   && Diffcessible.Zipper.get_before z = [] 
                   && Diffcessible.Zipper.get_after z = [2; 3; 4; 5]
                   && Diffcessible.Zipper.get_total_length z = List.length lst
                   && Diffcessible.Zipper.get_current_index z = 0
    in test_b0
  in let b1 = 
    let lst = [] in 
    try 
      let _ = candidate lst in 
      false
    with
      | Diffcessible.Zipper.Empty_list -> true
  in b0 && b1

(** a function to test the next function *)
let test_next_zipper candidate =
  let b0 = 
    let lst = [1; 2; 3; 4; 5] in
    try
      let z = Diffcessible.Zipper.zipper_of_list lst in
      let next_z = candidate z in 
      let test_b0 = Diffcessible.Zipper.get_focus next_z = 2 
                    && Diffcessible.Zipper.get_before next_z = [1] 
                    && Diffcessible.Zipper.get_after next_z = [3; 4; 5]
                    && Diffcessible.Zipper.get_total_length next_z = List.length lst
                    && Diffcessible.Zipper.get_current_index next_z = 1
      in test_b0
    with
      | Diffcessible.Zipper.Empty_list -> false
  in b0

(** a function to test the prev function *)
let test_previous_zipper candidate =
  let b0 = 
    let lst = [1; 2; 3; 4; 5] in
    try
      let z = Diffcessible.Zipper.zipper_of_list lst in
      let next_z = Diffcessible.Zipper.next z in
      let next_next_z = Diffcessible.Zipper.next next_z in
      let previous_z = candidate next_next_z in
      let test_b0 = Diffcessible.Zipper.get_focus previous_z = 2 
                    && Diffcessible.Zipper.get_before previous_z = [1] 
                    && Diffcessible.Zipper.get_after previous_z = [3; 4; 5]
                    && Diffcessible.Zipper.get_total_length previous_z = List.length lst
                    && Diffcessible.Zipper.get_current_index previous_z = 1
      in test_b0
    with
      | Diffcessible.Zipper.Empty_list -> false
  in b0

let () =
  assert (test_zipper_of_list Diffcessible.Zipper.zipper_of_list);
  assert (test_next_zipper Diffcessible.Zipper.next);
  assert (test_previous_zipper Diffcessible.Zipper.prev);
  print_endline "All tests passed!"
