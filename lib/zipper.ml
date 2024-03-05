type 'a t = {
  before : 'a list;
  focus : 'a;
  after : 'a list;
  total_length : int;
  current_index : int; (* 0 index *)
}

exception Empty_list

let zipper_of_list lst =
  match lst with
  | [] -> raise Empty_list
  | x :: xs -> { before = []; focus = x; after = xs; total_length = List.length lst; current_index = 0 }

let next z =
  match z.after with
  | [] -> z
  | x :: xs -> { z with before = z.focus :: z.before; focus = x; after = xs; current_index = z.current_index + 1 }

let prev z =
  match z.before with
  | [] -> z
  | x :: xs -> { z with before = xs; focus = x; after = z.focus :: z.after; current_index = z.current_index - 1 }

let get_focus z = z.focus

let get_before z = z.before

let get_after z = z.after

let get_total_length z = z.total_length

let get_current_index z = z.current_index

