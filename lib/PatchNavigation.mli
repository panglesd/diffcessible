(** This module defines navigation and summary functions for viewing changes in a diff viewer application using the Nottui library. It supports navigating between patches and displaying summaries of changes. *)

type direction =
  | Prev
  | Next
      (** Type representing navigation directions within a list of patches. 
          - [Prev] indicates navigation to the previous item.
          - [Next] indicates navigation to the next item. *)

(** [quit] is a variable indicating whether the application should terminate its main loop. *)

val navigate : 'a Zipper.t -> direction -> unit
(** [navigate z_patches dir] navigates within a zipper of patches in the specified direction [dir].
    It updates the zipper's state to the previous or next item based on the direction.
    @param z_patches A reactive variable containing a zipper of patches.
    @param dir The direction to navigate within the zipper.
    @return () Updates the state of [z_patches] without returning any value. *)

