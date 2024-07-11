type diff_line = Common of string | Mine of string | Their of string
type snake = (int * int) * (int * int)
type grid = { left : int; top : int; right : int; bottom : int }

let width (grid : grid) : int = grid.right - grid.left
let height (grid : grid) : int = grid.bottom - grid.top
let size (grid : grid) : int = width grid + height grid
let delta (grid : grid) : int = width grid - height grid

let longest (xs : 'a list) (ys : 'a list) : 'a list =
  if List.length xs > List.length ys then xs else ys

let get (arr : 'a array) (idx : int) : 'a =
  if idx < 0 || idx >= Array.length arr then
    failwith "Error: Index out of bounds"
  else arr.(idx)

let forwards (grid : grid) (vf : int array) (vb : int array) (d : int) :
    snake list =
  let rec step k =
    if k < -d then []
    else if k mod 2 = 0 then (
      let c = k - delta grid in
      let px, x =
        if k = -d || (k <> d && get vf (k - 1) < get vf (k + 1)) then
          let x = get vf (k + 1) in
          (x, x)
        else
          let px = get vf (k - 1) in
          (px, px + 1)
      in
      let y = grid.top + (x - grid.left) - k in
      let py = if d = 0 || x <> px then y else y - 1 in
      let rec advance x y =
        if
          x < grid.right && y < grid.bottom
          && x < Array.length vf
          && y < Array.length vb
          && vf.(x) = vb.(y)
        then advance (x + 1) (y + 1)
        else (x, y)
      in
      let x, y = advance x y in
      vf.(k) <- x;
      if delta grid mod 2 = 1 && c >= -(d - 1) && c <= d - 1 && y >= get vb c
      then [ ((px, py), (x, y)) ]
      else step (k - 2))
    else step (k - 2)
  in
  step d

let backward (grid : grid) (vf : int array) (vb : int array) (d : int) :
    snake list =
  let rec step c =
    if c < -d then []
    else if c mod 2 = 0 then (
      let k = c + delta grid in
      let py, y =
        if c = -d || (c <> d && get vb (c - 1) > get vb (c + 1)) then
          let y = get vb (c + 1) in
          (y, y)
        else
          let py = get vb (c - 1) in
          (py, py - 1)
      in
      let x = grid.left + (y - grid.top) + k in
      let px = if d = 0 || y <> py then x else x + 1 in
      let rec retreat x y =
        if
          x > grid.left && y > grid.top
          && x <= Array.length vf
          && y <= Array.length vb
          && vf.(x - 1) = vb.(y - 1)
        then retreat (x - 1) (y - 1)
        else (x, y)
      in
      let x, y = retreat x y in
      vb.(c) <- y;
      if delta grid mod 2 = 0 && k >= -d && k <= d && x <= get vf k then
        [ ((x, y), (px, py)) ]
      else step (c - 2))
    else step (c - 2)
  in
  step d

let midpoint (grid : grid) : snake option =
  if size grid = 0 then None
  else
    let max = int_of_float (ceil (float_of_int (size grid) /. 2.0)) in
    let vf = Array.make ((2 * max) + 1) 0 in
    let vb = Array.make ((2 * max) + 1) 0 in
    vf.(1) <- grid.left;
    vb.(1) <- grid.bottom;
    let rec search d =
      if d > max then None
      else
        match forwards grid vf vb d with
        | snake :: _ -> Some snake
        | [] -> (
            match backward grid vf vb d with
            | snake :: _ -> Some snake
            | [] -> search (d + 1))
    in
    search 0

let rec find_path (left : int) (top : int) (right : int) (bottom : int) :
    (int * int) list option =
  let grid = { left; top; right; bottom } in
  let snake =
    match midpoint grid with
    | None -> None
    | Some snake ->
        let start, finish = snake in
        let head = find_path left top (fst start) (snd start) in
        let tail = find_path (fst finish) (snd finish) right bottom in
        Some
          ((match head with None -> [ start ] | Some h -> h)
          @ match tail with None -> [ finish ] | Some t -> t)
  in
  snake

let a = [| "a"; "b"; "c"; "d"; "e" |]
let b = [| "a"; "b"; "x"; "d"; "e" |]

let () =
  match find_path 0 0 (Array.length a) (Array.length b) with
  | Some path -> List.iter (fun (x, y) -> Printf.printf "(%d, %d)\n" x y) path
  | None -> Printf.printf "No path found\n"
