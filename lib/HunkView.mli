(** This module provides functionalities for rendering different views of hunks within a patch in a diff viewer application using the Nottui library. It supports both unified and side-by-side display of changes. *)

val current_hunks : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [current_hunks z_patches] creates a reactive UI element to display all hunks in the current patch from a zipper of patches in a unified format.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element listing all hunks in the current patch. *)

val current_hunks_side_by_side : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [current_hunks_side_by_side z_patches] creates a reactive UI element to display all hunks in the current patch from a zipper of patches in a side-by-side format.
    This view aligns changes horizontally, allowing easier comparison of 'mine' and 'their' versions.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element listing all hunks in the current patch in side-by-side view. *)

type view_mode =
  | SideBySide
  | Normal
      (** Defines available view modes for displaying diffs: either side-by-side or as a unified view. *)

val view_mode : view_mode Lwd.var
(** Holds the current view mode of the diff display. *)

val toggle_view_mode : unit -> unit
(** Toggles the current view mode between side-by-side and normal views. Updates the [view_mode] variable. *)
