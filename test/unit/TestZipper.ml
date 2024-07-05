open Diffcessible

let test_zipper_of_list candidate =
  let b0 =
    let lst = [ 1; 2; 3; 4; 5 ] in
    match candidate lst with
    | None -> false
    | Some z ->
        let test_b0 =
          Zipper.get_focus z = 1
          && Zipper.get_before z = []
          && Zipper.get_after z = [ 2; 3; 4; 5 ]
          && Zipper.get_total_length z = List.length lst
          && Zipper.get_current_index z = 0
        in
        test_b0
  in
  let b1 =
    let lst = [] in
    match candidate lst with None -> true | _ -> false
  in
  b0 && b1

let test_next_zipper candidate =
  let b0 =
    let lst = [ 1; 2; 3; 4; 5 ] in
    let z = Zipper.zipper_of_list lst in
    match z with
    | None -> false
    | Some z ->
        let next_z = candidate z in
        let test_b0 =
          Zipper.get_focus next_z = 2
          && Zipper.get_before next_z = [ 1 ]
          && Zipper.get_after next_z = [ 3; 4; 5 ]
          && Zipper.get_total_length next_z = List.length lst
          && Zipper.get_current_index next_z = 1
        in
        test_b0
  in
  b0

let test_previous_zipper candidate =
  let b0 =
    let lst = [ 1; 2; 3; 4; 5 ] in
    let z = Zipper.zipper_of_list lst in
    match z with
    | None -> false
    | Some z ->
        let next_z = Zipper.next z in
        let next_next_z = Zipper.next next_z in
        let previous_z = candidate next_next_z in
        let test_b0 =
          Zipper.get_focus previous_z = 2
          && Zipper.get_before previous_z = [ 1 ]
          && Zipper.get_after previous_z = [ 3; 4; 5 ]
          && Zipper.get_total_length previous_z = List.length lst
          && Zipper.get_current_index previous_z = 1
        in
        test_b0
  in
  b0

let () =
  assert (test_zipper_of_list Zipper.zipper_of_list);
  assert (test_next_zipper Zipper.next);
  assert (test_previous_zipper Zipper.prev);
  print_endline "All tests passed!"
