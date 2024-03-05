(** This file contains the zipper implementation *)

module Zipper : sig
  (** The type of a zipper. A zipper is a list with a focus element. *)
  type 'a t

  (** Exception raised when trying to create a zipper from an empty list. *)
  exception Empty_list

  (** [zipper_of_list lst] creates a zipper from a list. The first element of the list becomes the focus of the zipper. *)
  val zipper_of_list : 'a list -> 'a t 


  (** [next zipper] advances the zipper to the next element. If there is no next element, the zipper remains unchanged. *)
  val next : 'a t -> 'a t

  (** [prev zipper] moves the zipper to the previous element. If there is no previous element, the zipper remains unchanged. *)
  val prev : 'a t -> 'a t

  (** [get_focus zipper] extracts the current focus element of the zipper. *)
  val get_focus : 'a t -> 'a

  (** [get_before zipper] extracts the list of elements before the focus element of the zipper. *)
  val get_before : 'a t -> 'a list

  (** [get_after zipper] extracts the list of elements after the focus element of the zipper. *)
  val get_after : 'a t -> 'a list
end = struct
  type 'a t = {
    before : 'a list;
    focus : 'a;
    after : 'a list;
  }

  exception Empty_list

  let zipper_of_list lst =
    match lst with
    | [] -> raise Empty_list
    | x :: xs -> { before = []; focus = x; after = xs }

  let next z =
    match z.after with
    | [] -> z
    | x :: xs -> { before = z.focus :: z.before; focus = x; after = xs }

  let prev z =
    match z.before with
    | [] -> z
    | x :: xs -> { before = xs; focus = x; after = z.focus :: z.after }

  let get_focus z = z.focus

  let get_before z = z.before

  let get_after z = z.after
end
