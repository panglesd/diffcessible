type word = Unchanged of string | Changed of string
type line_content = word list

val compute : string Block.t -> line_content Block.t
