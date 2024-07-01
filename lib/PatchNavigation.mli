(** This module defines navigation and summary functions for viewing changes in a diff viewer application using the Nottui library. It provides utilities to navigate between patches, calculate additions and removals, and display summaries of these changes. *)

type direction =
  | Prev
  | Next
      (** Type representing navigation directions within a list of patches. 
    - [Prev] indicates navigation to the previous item.
    - [Next] indicates navigation to the next item. *)

val navigate : 'a Zipper.t Lwd.var -> direction -> unit
(** [navigate z_patches dir] navigates within a zipper of patches in the specified direction [dir].
    It updates the zipper's state to the previous or next item based on the direction.
    @param z_patches A reactive variable containing a zipper of patches.
    @param dir The direction to navigate within the zipper.
    @return () Updates the state of [z_patches] without returning any value. *)

val quit : bool Lwd.var
(** [quit] is a reactive variable indicating whether the application should terminate its main loop. *)

val additions_and_removals :
  [< `Common of 'a | `Mine of 'b | `Their of 'c ] list -> int * int
(** [additions_and_removals lines] calculates the number of additions and removals in a list of lines.
    - Lines tagged as [`Their] count as additions.
    - Lines tagged as [`Mine] count as removals.
    - Lines tagged as [`Common] are ignored in the count.
    @param lines A list of lines with specific tags indicating their type.
    @return (additions, removals) A tuple with the count of additions and removals. *)

val accumulate_count : Patch.hunk list -> int * int
(** [accumulate_count hunks] calculates the total number of additions and removals across a list of hunks.
    This function aggregates the counts by applying [additions_and_removals] to each hunk's lines.
    @param hunks A list of hunks to process.
    @return (total_additions, total_removals) A tuple with the aggregated counts of additions and removals. *)

val change_summary : Patch.t Zipper.t Lwd.var -> Nottui.ui Lwd.t
(** [change_summary z_patches] generates a reactive UI element that displays a summary of changes for the current patch in the zipper.
    It shows the total number of additions and removals formatted in a readable format.
    @param z_patches A reactive variable containing a zipper of patches.
    @return A reactive UI element displaying the summary of changes. *)
