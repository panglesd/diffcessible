val navigate :
  Types.navigation_direction -> 'a Patch.t Zipper.t -> 'a Patch.t Zipper.t
(** [navigate direction zipper] returns the zipper that is the result of
    navigating in the given direction. *)
