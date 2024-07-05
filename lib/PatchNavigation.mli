type direction =
  | Prev
  | Next  (** [direction] represents the direction of navigation. *)

val navigate : direction -> Patch.t Zipper.t -> Patch.t Zipper.t
(** [navigate direction zipper] returns the zipper that is the result of
    navigating in the given direction. *)
