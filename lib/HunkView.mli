(** This module provides functionalities for rendering different views of hunks within a patch in a diff viewer application using the Nottui library. It supports both unified and side-by-side display of changes. *)

val ui_hunk_summary : Patch.hunk -> Nottui.ui
(** [ui_hunk_summary hunk] creates a UI representation of the summary for a single hunk.
    It displays the start and length of changes in both 'mine' and 'their' versions of the text.
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

type view_mode =
  | SideBySide
  | Normal
      (** Type that defines available view modes for displaying diffs: either side-by-side or as a unified list. *)

val view_mode : view_mode Lwd.var
(** [view_mode] is a reactive variable that holds the current view mode of the diff display, allowing dynamic switching between view modes. *)

val toggle_view_mode : unit -> unit
(** [toggle_view_mode ()] toggles the current view mode between side-by-side and normal views. This function updates the [view_mode] variable. *)

type line =
  | Change of string
  | Common of string
  | Empty
      (** Type representing a line in a diff view:
    - [Change string] denotes a modified line.
    - [Common string] denotes an unchanged line.
    - [Empty] denotes a spacer line for alignment purposes in side-by-side views. *)

val split_and_align_hunk :
  [< `Common of string | `Mine of string | `Their of string > `Common ] list ->
  line list ->
  line list ->
  line list * line list
(** [split_and_align_hunk hunks mine_acc their_acc] processes a list of lines from a hunk and aligns them for side-by-side display, balancing the line counts between two columns.
    @param hunks A list of lines tagged with their types (common, mine, or their).
    @param mine_acc An accumulator for 'mine' lines.
    @param their_acc An accumulator for 'their' lines.
    @return A tuple of aligned lists of lines for both 'mine' and 'their'. *)

val lines_with_numbers : line list -> Notty.attr -> string -> Nottui.ui list
(** [lines_with_numbers lines attr_change prefix] generates a list of UI elements for lines of text, each prefixed and styled according to its type.
    @param lines A list of lines to be displayed.
    @param attr_change Attributes to apply to changed lines.
    @param prefix A string prefix to prepend before each line (like "+", "-", or " " for common lines).
    @return A list of UI elements where each element corresponds to a line of text with appropriate styling and prefix. *)

val create_summary :
  int -> int -> Notty.attr -> [< `Add | `Remove ] -> Nottui.ui
(** [create_summary start_line_num hunk_length attr change_type] creates a summary UI element for a hunk indicating the number of added or removed lines.
    @param start_line_num The starting line number of the hunk.
    @param hunk_length The number of lines in the hunk.
    @param attr Attributes to style the summary text.
    @param change_type Indicates whether the lines were added or removed.
    @return A UI element representing the summary of changes. *)

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
