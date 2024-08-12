type word = Unchanged of string | Changed of string
type line_content = word list

val compute : string Block.t -> line_content Block.t

(* for tests *)
val lcs : 'a list -> 'a list -> 'a list
val diff_words : string -> string -> Types.line_content * Types.line_content
