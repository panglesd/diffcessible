type 'a t
(** Abstract type for a zipper for elements of type ['a] that allows for efficient traversal of a list. *)

val zipper_of_list : 'a list -> 'a t option
(** [zipper_of_list lst] converts a list into a zipper. If the list is empty, it returns [None].
    Otherwise, it returns [Some zipper] with the first element of the list as the focus. *)

val next : 'a t -> 'a t
(** [next z] moves the focus of the zipper [z] to the next element, if any.
    If the focus is on the last element, the zipper remains unchanged. *)

val prev : 'a t -> 'a t
(** [prev z] moves the focus of the zipper [z] to the previous element, if any.
    If the focus is on the first element, the zipper remains unchanged. *)

val get_focus : 'a t -> 'a
(** [get_focus z] returns the current focus of the zipper [z]. *)

val get_before : 'a t -> 'a list
(** [get_before z] returns the list of elements before the focus in the zipper [z]. *)

val get_after : 'a t -> 'a list
(** [get_after z] returns the list of elements after the focus in the zipper [z]. *)

val get_total_length : 'a t -> int
(** [get_total_length z] returns the total length of the list represented by the zipper [z]. *)

val get_current_index : 'a t -> int
(** [get_current_index z] returns the current index (0-based) of the focus in the list. *)

type 'a t
(** Abstract type for a zipper for elements of type ['a] that allows for efficient traversal of a list. *)

val zipper_of_list : 'a list -> 'a t option
(** [zipper_of_list lst] converts a list into a zipper. If the list is empty, it returns [None].
    Otherwise, it returns [Some zipper] with the first element of the list as the focus. *)

val next : 'a t -> 'a t
(** [next z] moves the focus of the zipper [z] to the next element, if any.
    If the focus is on the last element, the zipper remains unchanged. *)

val prev : 'a t -> 'a t
(** [prev z] moves the focus of the zipper [z] to the previous element, if any.
    If the focus is on the first element, the zipper remains unchanged. *)

val get_focus : 'a t -> 'a
(** [get_focus z] returns the current focus of the zipper [z]. *)

val get_before : 'a t -> 'a list
(** [get_before z] returns the list of elements before the focus in the zipper [z]. *)

val get_after : 'a t -> 'a list
(** [get_after z] returns the list of elements after the focus in the zipper [z]. *)

val get_total_length : 'a t -> int
(** [get_total_length z] returns the total length of the list represented by the zipper [z]. *)

val get_current_index : 'a t -> int
(** [get_current_index z] returns the current index (0-based) of the focus in the list. *)
