module WordDiff : sig
  type item = string

  type diff = Simple_diff.Make(String).diff =
    | Deleted of item array
    | Added of item array
    | Equal of item array

  type t = diff list

  val get_diff : item array -> item array -> t
  (** [get_diff arr1 arr2] computes the word-level diff between [arr1] and [arr2] *)
end

val string_to_words : string -> string array
(** [string_to_words s] splits the string [s] into an array of words *)

val words_to_string : string array -> string
(** [words_to_string arr] joins an array of words into a single string *)

val diff_words : string -> string -> WordDiff.t
(** [diff_words s1 s2] computes the word-level diff between strings [s1] and [s2] *)

type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array
      (** [word_diff] represents the result of a word-level diff operation *)

val apply_word_diff : string -> string -> word_diff list
(** [apply_word_diff s1 s2] applies word-level diffing to strings [s1] and [s2]
    and returns a list of word_diff elements *)
