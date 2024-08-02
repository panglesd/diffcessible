type word = Unchanged of string | Changed of string
type line_content = word list

val compute : string Block.t -> line_content Block.t
val render_hunk_lines : line_content Patch.line list -> Nottui.ui
val render_hunk : string Patch.hunk -> Nottui.Ui.t

(* for tests *)
val lcs : 'a list -> 'a list -> 'a list
val diff_words : string -> string -> line_content * line_content
