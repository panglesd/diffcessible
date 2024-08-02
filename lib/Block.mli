type block_origin = Mine | Their | None
type 'a block_content = Entry of 'a | Newline

type 'a t =
  | Common of 'a block_content
  | Changed of {
      mine : 'a block_content list;
      their : 'a block_content list;
      order : block_origin;
    }

val of_hunk : 'a Patch.line list -> 'a t list

(* Turns a hunk into a list of blocks by grouping consecutive changed lines. *)
val to_hunk : 'a t list -> 'a Patch.line list
(* Turns back a list of blocks into a hunk.
       [x |> of_hunk |> to_hunk] should yield [x] back. *)
