type snake = (int * int) * (int * int)
type edit = Common of string | Mine of string | Their of string
type box = { left : int; top : int; right : int; bottom : int }

let width box = box.right - box.left
let height box = box.bottom - box.top
let size box = width box + height box
let delta box = width box - height box

let assert_index bounds idx =
  if idx >= 0 && idx < bounds then idx
  else raise (Invalid_argument "index out of bounds")

let rec midpoint box a b =
  Printf.printf "midpoint called with box: %d, %d, %d, %d\n" box.left box.top
    box.right box.bottom;
  if size box = 0 then (
    Printf.printf "size of box is 0, returning None\n";
    None)
  else
    let max = (size box + 1) / 2 in
    let vf = Array.make ((2 * max) + 1) box.left in
    let vb = Array.make ((2 * max) + 1) box.bottom in
    let check_snake d =
      match forwards box vf vb d a b with
      | Some snake ->
          Printf.printf "  forwards returned Some snake\n";
          Some snake
      | None -> (
          Printf.printf "  forwards returned None\n";
          match backward box vf vb d a b with
          | Some snake ->
              Printf.printf "  backward returned Some snake\n";
              Some snake
          | None ->
              Printf.printf "  backward returned None\n";
              None)
    in
    let result = ref None in
    for d = 0 to max do
      if !result = None then result := check_snake d
    done;
    !result

and forwards box vf vb d a b =
  Printf.printf "forwards called with d = %d\n" d;
  let max_d = (Array.length vf - 1) / 2 in
  let result = ref None in
  for k = -d to d do
    if !result = None && k mod 2 = 0 then
      Printf.printf "  forwards loop called with k = %d\n" k;
    let x =
      if k = -d || (k <> d && vf.(k - 1 + max_d) < vf.(k + 1 + max_d)) then
        vf.(k + 1 + max_d)
      else vf.(k - 1 + max_d) + 1
    in
    let px = if k <> -d && x = vf.(k + 1 + max_d) then x - 1 else x in
    let y = box.top + (x - box.left) - k in
    let py = if d = 0 || x <> px then y else y - 1 in
    let rec diagonal x y =
      if
        x < box.right && y < box.bottom
        && assert_index (List.length a) x = assert_index (List.length b) y
      then diagonal (x + 1) (y + 1)
      else (x, y)
    in
    let x, y = diagonal x y in
    vf.(k + max_d) <- x;
    if
      delta box mod 2 = 1
      && k >= -d + 1
      && k <= d - 1
      && y >= vb.(k - delta box + max_d)
    then (
      Printf.printf "  found a snake, returning Some ((px, py), (x, y))\n";
      result := Some ((px, py), (x, y)))
  done;
  !result

and backward box vf vb d a b =
  Printf.printf "backward called with d = %d\n" d;
  let max_d = (Array.length vb - 1) / 2 in
  let result = ref None in
  for k = -d to d do
    if !result = None && k mod 2 = 0 then
      Printf.printf "  backward loop called with k = %d\n" k;
    let y =
      if k = -d || (k <> d && vb.(k - 1 + max_d) > vb.(k + 1 + max_d)) then
        vb.(k + 1 + max_d)
      else vb.(k - 1 + max_d) - 1
    in
    let py = if k <> -d && y = vb.(k + 1 + max_d) then y + 1 else y in
    let x = box.left + (y - box.top) + k in
    let px = if d = 0 || y <> py then x else x + 1 in
    let rec diagonal x y =
      if
        x > box.left && y > box.top
        && assert_index (List.length a) (x - 1)
           = assert_index (List.length b) (y - 1)
      then diagonal (x - 1) (y - 1)
      else (x, y)
    in
    let x, y = diagonal x y in
    vb.(k + max_d) <- y;
    if
      delta box mod 2 = 0
      && k >= -d && k <= d
      && x <= vf.(k + delta box + max_d)
    then (
      Printf.printf "  found a snake, returning Some ((x, y), (px, py))\n";
      result := Some ((x, y), (px, py)))
  done;
  !result

let rec find_path a b left top right bottom =
  Printf.printf "find_path called with left=%d, top=%d, right=%d, bottom=%d\n"
    left top right bottom;
  let box = { left; top; right; bottom } in
  match midpoint box a b with
  | None ->
      Printf.printf "midpoint returned None\n";
      []
  | Some ((start_x, start_y), (finish_x, finish_y)) -> (
      Printf.printf
        "midpoint returned Some ((start_x, start_y), (finish_x, finish_y))\n";
      let head = find_path a b box.left box.top start_x start_y in
      let tail = find_path a b finish_x finish_y box.right box.bottom in
      (match head with
      | [] -> [ (start_x, start_y) ]
      | _ -> (start_x, start_y) :: head)
      @
      match tail with
      | [] -> [ (finish_x, finish_y) ]
      | _ -> tail @ [ (finish_x, finish_y) ])

let walk_snakes a b =
  let rec walk_diagonal x1 y1 x2 y2 acc =
    if
      x1 < x2 && y1 < y2
      && assert_index (List.length a) x1 = assert_index (List.length b) y1
    then walk_diagonal (x1 + 1) (y1 + 1) x2 y2 ((x1, y1, x1 + 1, y1 + 1) :: acc)
    else (x1, y1, acc)
  in
  let rec loop = function
    | [] ->
        Printf.printf "loop called with empty list\n";
        []
    | [ (_, _) ] ->
        Printf.printf "loop called with singleton list\n";
        []
    | (x1, y1) :: (x2, y2) :: rest ->
        Printf.printf
          "loop called with (x1, y1) = (%d, %d) and (x2, y2) = (%d, %d)\n" x1 y1
          x2 y2;
        let x1', y1', steps = walk_diagonal x1 y1 x2 y2 [] in
        let steps =
          match (x2 - x1', y2 - y1') with
          | 0, 1 ->
              Printf.printf "  downward step\n";
              (x1', y1', x1', y1' + 1) :: steps
          | 1, 0 ->
              Printf.printf "  rightward step\n";
              (x1', y1', x1' + 1, y1') :: steps
          | _ ->
              Printf.printf "  diagonal step\n";
              steps
        in
        steps @ loop ((x2, y2) :: rest)
  in
  loop (find_path a b 0 0 (List.length a) (List.length b))

let diff a b =
  let rec loop = function
    | [] ->
        Printf.printf "diff loop called with empty list\n";
        []
    | (x1, y1, x2, y2) :: rest ->
        Printf.printf
          "diff loop called with (x1, y1, x2, y2) = (%d, %d, %d, %d)\n" x1 y1 x2
          y2;
        if x1 = x2 then (
          let line = List.nth b y1 in
          Printf.printf "  x1 = x2, adding Their (List.nth b %d) = %s\n" y1 line;
          Their line :: loop rest)
        else if y1 = y2 then (
          let line = List.nth a x1 in
          Printf.printf "  y1 = y2, adding Mine (List.nth a %d) = %s\n" x1 line;
          Mine line :: loop rest)
        else
          let line = List.nth a x1 in
          Printf.printf "  adding Common (List.nth a %d) = %s\n" x1 line;
          Common line :: loop rest
  in
  loop (walk_snakes a b)

(* Example usage *)
let old_str = "Foo\nbar\nbaz"
let new_str = "Foo\nbaz\nbar"
let old_lines = String.split_on_char '\n' old_str
let new_lines = String.split_on_char '\n' new_str
let diffs = diff old_lines new_lines

let () =
  List.iter
    (function
      | Common line -> Printf.printf "  %s\n" line
      | Mine line -> Printf.printf "- %s\n" line
      | Their line -> Printf.printf "+ %s\n" line)
    diffs
