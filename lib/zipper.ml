(** This file contains the zipper implementation *)

type 'a zipper = {
  before : 'a list;
  focus : 'a;
  after : 'a list;
}

(** a function to initialize a zipper given a list *)
let zipper_of_list (lst : 'a list)  : ('a zipper option) = 
  match lst with
  | [] -> None
  | x :: xs -> Some {
    before = [];
    focus = x;
    after = xs;
  }

(** a function to peal open the option type of 'a zipper option *)

let get_zipper_of_list (z : 'a zipper option) : 'a zipper = 
  match z with
  | None -> failwith "No zipper found!"
  | Some z -> z

(** a function to access the next element in a zipper *)
let next_zipper z = match z.after with
  | [] ->
      z 
  | x :: xs ->
      {
      before = z.focus :: z.before;
      focus = x;
      after = xs
    }

(** a function to access the previous element in a zipper *)
let prev_zipper z = match z.before with
  | [] ->
      z
  | x :: xs ->
    let focus' = z.focus in
    {
      before = xs;
      focus = x;
      after = focus' :: z.after
    }

