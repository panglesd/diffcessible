val compute : string Block.t -> Types.line_content Block.t

val render_hunk_lines :
  Types.line_content Patch.line list -> Types.rendering_mode -> Nottui.ui

val render_hunk : string Patch.hunk -> Types.rendering_mode -> Nottui.ui

(* for tests *)
val lcs : 'a list -> 'a list -> 'a list
val diff_words : string -> string -> Types.line_content * Types.line_content
