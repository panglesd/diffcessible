type word = Unchanged of string | Changed of string
type line_content = word list

val compute : string Block.t -> line_content Block.t
val render_hunk_lines : line_content Patch.line list -> Nottui.ui
val render_hunk : string Patch.hunk -> Nottui.Ui.t
(* type block_origin = Mine | Their | None
   type 'a block_content = Entry of 'a | Newline

   type 'a t =
     | Common of 'a block_content
     | Changed of {
         mine : 'a block_content list;
         their : 'a block_content list;
         order : block_origin;
       } *)
