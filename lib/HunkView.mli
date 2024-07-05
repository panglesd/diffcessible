val current_hunks : Patch.t Zipper.t -> Nottui.ui
(** [current_hunks zipper] returns the current hunks in a patch zipper for normal view. *)

val current_hunks_side_by_side : Patch.t Zipper.t -> Nottui.ui
(** [current_hunks_side_by_side zipper] returns the current hunks in a patch zipper for side-by-side view. *)
