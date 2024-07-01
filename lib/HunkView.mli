(** This module provides functionalities for rendering different views of hunks within a patch in a diff viewer application using the Nottui library. It supports both unified and side-by-side display of changes. *)

val ui_hunk_summary : Patch.hunk -> Nottui.ui
(** [ui_hunk_summary hunk] creates a UI representation of the summary for a single hunk.
    Displays the start and length of changes in both 'mine' and 'their' versions of the text.
    @param hunk A hunk containing details like start position and length of changes.
    @return A UI element that visually summarizes the hunk's details. *)

val ui_unified_diff : Patch.hunk -> Nottui.ui
(** [ui_unified_diff hunk] generates a UI element that displays a unified diff of a single hunk.
    It lists changes line by line, marking additions and deletions with appropriate styles.
    @param hunk A hunk containing lines with changes.
    @return A UI element that shows a unified view of the hunk's changes. *)

val current_hunks : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [current_hunks z_patches] creates a reactive UI element to display all hunks in the current patch from a zipper of patches in a unified format.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element listing all hunks in the current patch. *)

val ui_of_hunk_side_by_side : Patch.hunk -> Nottui.ui
(** [ui_of_hunk_side_by_side hunk] constructs a UI element that displays the differences of a single hunk in a side-by-side layout.
    This format separates additions and deletions visually, aligning them horizontally.
    @param hunk A hunk to be displayed.
    @return A UI element showing the hunk's changes in a side-by-side format. *)

val current_hunks_side_by_side : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [current_hunks_side_by_side z_patches] creates a reactive UI element to display all hunks in the current patch from a zipper of patches in a side-by-side format.
    This view aligns changes horizontally, allowing easier comparison of 'mine' and 'their' versions.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element listing all hunks in the current patch in side-by-side view. *)

type view_mode = SideBySide | Normal
(** Defines available view modes for displaying diffs: either side-by-side or as a unified view. *)

val view_mode : view_mode Lwd.var
(** Holds the current view mode of the diff display. *)

val toggle_view_mode : unit -> unit
(** Toggles the current view mode between side-by-side and normal views. Updates the [view_mode] variable. *)

