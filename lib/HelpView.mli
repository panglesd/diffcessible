(** This module provides UI components and attributes for a help panel. *)

val help_visible : bool Lwd.var
(** [help_visible] is a variable indicating whether the help panel is currently visible. *)

val help_panel : Nottui.ui
(** [help_panel] constructs the user interface for the help panel, containing various help commands with corresponding attributes. *)

val help_keyboard_area : Nottui.ui
(** [help_keyboard_area] provides a keyboard interaction area where pressing 'q' will close the help panel. *)

val toggle_help_visibility : unit -> unit
(** [toggle_help_visibility ()] toggles the visibility of the help panel. It flips the current state of [help_visible]. *)
