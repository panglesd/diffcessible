type direction =
  | Prev
  | Next  (** [direction] represents the direction of navigation. *)

val navigate : direction -> 'a Patch.t Zipper.t -> 'a Patch.t Zipper.t
(** [navigate direction zipper] returns the zipper that is the result of
    navigating in the given direction. *)
