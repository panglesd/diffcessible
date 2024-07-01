(** This module provides UI components and attributes for a help panel in a Nottui-based application. *)

val help_visible : bool Lwd.var
(** [help_visible] is a variable indicating whether the help panel is currently visible. *)

val help_panel : Nottui.ui
(** [help_panel] constructs the user interface for the help panel. This includes various help-related commands and their corresponding visual attributes, making it easy for users to understand available interactions. *)

val toggle_help_visibility : unit -> unit
(** [toggle_help_visibility ()] toggles the visibility of the help panel. This function modifies the state of [help_visible], effectively showing or hiding the help panel based on its current state. *)
