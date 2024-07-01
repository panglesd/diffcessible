(** This module provides UI components and attributes for a help panel in a Nottui-based application. *)
module W = Nottui_widgets
(** Module alias for Nottui_widgets for easier access to widget creation functions. *)

val header_color : Notty.attr
(** [header_color] defines the Notty attribute for header texts in the help panel, characterized by a light red foreground and bold style. *)

val action_color : Notty.attr
(** [action_color] defines the Notty attribute for action items in the help panel, characterized by a green foreground. *)

val info_color : Notty.attr
(** [info_color] defines the Notty attribute for informational texts in the help panel, characterized by a yellow foreground. *)

val help_visible : bool Lwd.var
(** [help_visible] is a reactive variable indicating whether the help panel is currently visible. *)

val help_panel : Nottui.ui
(** [help_panel] constructs the user interface for the help panel, containing various help commands with corresponding attributes. *)

val help_keyboard_area : Nottui.ui
(** [help_keyboard_area] provides a keyboard interaction area where pressing 'q' will close the help panel. *)

val toggle_help_visibility : unit -> unit
(** [toggle_help_visibility ()] toggles the visibility of the help panel. It flips the current state of [help_visible]. *)
