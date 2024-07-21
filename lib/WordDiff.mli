type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

type line_diff =
  | CommonDiff of string
  | DeletedDiff of word_diff list
  | AddedDiff of word_diff list
  | ModifiedDiff of word_diff list * word_diff list

module Diff : sig
  val apply_word_diff : string -> string -> word_diff list
  (** [apply_word_diff old new] returns a list of word_diffs that can be used to
      compare [mine] and [their] strings *)
end

val render_line_diff : int -> int -> line_diff -> int * int * Nottui.ui
(** [render_line_diff width height diff] renders a line_diff into a Nottui.ui *)
