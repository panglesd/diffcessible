(** The type of a zipper for elements of type ['a]. It consists of three parts:
    - [before]: The list of elements before the current focus.
    - [focus]: The current focused element of the zipper.
    - [after]: The list of elements after the focus.
    - [total_length]: The total length of the list represented by the zipper.
    - [current_index]: The current index (0-based) of the focus in the list. *)

type 'a t = {
  before : 'a list;
  focus : 'a;
  after : 'a list;
  total_length : int;
  current_index : int; (* 0-based index *)
}

(** [zipper_of_list lst] converts a list into a zipper. If the list is empty, it
    returns [None]. Otherwise, it returns [Some zipper] with the first element
    of the list as the focus. *)
let zipper_of_list lst : 'a t option =
  match lst with
  | [] -> None
  | x :: xs ->
      Some
        {
          before = [];
          focus = x;
          after = xs;
          total_length = List.length lst;
          current_index = 0;
        }

(** [next z] moves the focus of the zipper [z] to the next element, if any. If
    the focus is on the last element, the zipper remains unchanged. *)
let next z =
  match z.after with
  | [] -> z
  | x :: xs ->
      {
        z with
        before = z.focus :: z.before;
        focus = x;
        after = xs;
        current_index = z.current_index + 1;
      }

(** [prev z] moves the focus of the zipper [z] to the previous element, if any.
    If the focus is on the first element, the zipper remains unchanged. *)
let prev z =
  match z.before with
  | [] -> z
  | x :: xs ->
      {
        z with
        before = xs;
        focus = x;
        after = z.focus :: z.after;
        current_index = z.current_index - 1;
      }

(** [get_focus z] returns the current focus of the zipper [z]. *)
let get_focus z = z.focus

(** [get_before z] returns the list of elements before the focus in the zipper
    [z]. *)
let get_before z = z.before

(** [get_after z] returns the list of elements after the focus in the zipper
    [z]. *)
let get_after z = z.after

(** [get_total_length z] returns the total length of the list represented by the
    zipper [z]. *)
let get_total_length z = z.total_length

(** [get_current_index z] returns the current index of the focus in the zipper
    [z]. *)
let get_current_index z = z.current_index
