type snake = (int * int) * (int * int)
type diff_line = Common of string | Mine of string | Their of string
type box = { left : int; top : int; right : int; bottom : int }

let width box = box.right - box.left
let height box = box.bottom - box.top
let size box = width box + height box
let delta box = width box - height box

let assert_index bounds idx =
  if idx >= 0 && idx < bounds then idx
  else raise (Invalid_argument "index out of bounds")

let rec find_path a b left top right bottom =
  let box = { left; top; right; bottom } in
  let snake = midpoint box a b in
  match snake with
  | None -> []
  | Some ((start_x, start_y), (finish_x, finish_y)) ->
      let head = find_path a b box.left box.top start_x start_y in
      let tail = find_path a b finish_x finish_y box.right box.bottom in
      head @ [snake] @ tail

and midpoint box a b =
  if size box = 0 then None
  else
    let max = (size box + 1) / 2 in
    let vf = Array.make ((2 * max) + 1) box.left in
    let vb = Array.make ((2 * max) + 1) box.bottom in
    vf.(max + 1) <- box.left;
    vb.(max + 1) <- box.bottom;
    let rec loop d =
      if d > max then None
      else
        match forwards box vf vb d a b with
        | Some snake -> Some snake
        | None -> (
            match backward box vf vb d a b with
            | Some snake -> Some snake
            | None -> loop (d + 1))
    in
    loop 0

and forwards box vf vb d a b =
  let max_d = (Array.length vf - 1) / 2 in
  let rec loop k =
    if k > d then None
    else
      let k = d - k in
      let c = k - delta box in
      let x =
        if k = -d || (k <> d && vf.(k - 1 + max_d) < vf.(k + 1 + max_d)) then
          vf.(k + 1 + max_d)
        else vf.(k - 1 + max_d) + 1
      in
      let y = box.top + (x - box.left) - k in
      let px = if k = -d || (k <> d && vf.(k - 1 + max_d) < vf.(k + 1 + max_d)) then x else x - 1 in
      let py = if d = 0 || x <> px then y else y - 1 in
      let rec diagonal x y =
        if x < box.right && y < box.bottom && List.nth a x = List.nth b y then diagonal (x + 1) (y + 1)
        else (x, y)
      in
      let x, y = diagonal x y in
      vf.(k + max_d) <- x;
      if delta box mod 2 = 1 && c >= -d + 1 && c <= d - 1 && y >= vb.(c + max_d) then Some ((px, py), (x, y))
      else loop (k + 2)
  in
  loop (-d)

and backward box vf vb d a b =
  let max_d = (Array.length vb - 1) / 2 in
  let rec loop k =
    if k > d then None
    else
      let k = d - k in
      let c = k + delta box in
      let y =
        if k = -d || (k <> d && vb.(k - 1 + max_d) > vb.(k + 1 + max_d)) then
          vb.(k + 1 + max_d)
        else vb.(k - 1 + max_d) - 1
      in
      let x = box.left + (y - box.top) + k in
      let py = if k = -d || (k <> d && vb.(k - 1 + max_d) > vb.(k + 1 + max_d)) then y else y + 1 in
      let px = if d = 0 || y <> py then x else x + 1 in
      let rec diagonal x y =
        if x > box.left && y > box.top && List.nth a (x - 1) = List.nth b (y - 1) then diagonal (x - 1) (y - 1)
        else (x, y)
      in
      let x, y = diagonal x y in
      vb.(k + max_d) <- y;
      if delta box mod 2 = 0 && c >= -d && c <= d && x <= vf.(c + max_d) then Some ((x, y), (px, py))
      else loop (k + 2)
  in
  loop (-d)

let walk_snakes a b =
  let rec walk_diagonal x1 y1 x2 y2 acc =
    if x1 < x2 && y1 < y2 && List.nth a x1 = List.nth b y1 then
      walk_diagonal (x1 + 1) (y1 + 1) x2 y2 ((x1, y1, x1 + 1, y1 + 1) :: acc)
    else acc
  in
  let rec aux path acc =
    match path with
    | [] -> List.rev acc
    | ((x1, y1), (x2, y2)) :: rest ->
        let acc = walk_diagonal x1 y1 x2 y2 acc in
        if x1 = x2 then aux rest ((x1, y1, x1, y1 + 1) :: acc)
        else if y1 = y2 then aux rest ((x1, y1, x1 + 1, y1) :: acc)
        else aux rest acc
  in
  aux (find_path a b 0 0 (List.length a) (List.length b)) []


let diff a b =
  let rec loop acc = function
    | [] -> List.rev acc
    | (x1, y1, x2, y2) :: rest ->
        let acc =
          if x1 = x2 then Their (List.nth b y1) :: acc
          else if y1 = y2 then Mine (List.nth a x1) :: acc
          else Common (List.nth a x1) :: acc
        in
        loop acc rest
  in
  loop [] (walk_snakes a b)

let word_diff (old_str : string) (new_str : string) : string =
  let old_words : string list = String.split_on_char ' ' old_str in
  let new_words : string list = String.split_on_char ' ' new_str in
  let diff_lines : diff_line list = diff old_words new_words in

  let rec build_diff_string (acc : string list) (diff_lines : diff_line list) : string =
    match diff_lines with
    | [] -> String.concat "\n" (List.rev acc)
    | Common word :: rest -> build_diff_string (word :: acc) rest
    | Mine word :: Their new_word :: rest ->
        let line : string = Printf.sprintf "- %s\n+ %s" word new_word in
        build_diff_string (line :: acc) rest
    | Mine word :: rest ->
        let line : string = Printf.sprintf "- %s" word in
        build_diff_string (line :: acc) rest
    | Their word :: rest ->
        let line : string = Printf.sprintf "+ %s" word in
        build_diff_string (line :: acc) rest
  in

  build_diff_string [] diff_lines

let old_str : string = "Foo bar baz"
let new_str : string = "Foo baz bar"
let diff : string = word_diff old_str new_str
let () = print_endline diff

