type origin = Mine | Their

type 'a t =
  | Common of 'a
  | Changed of { mine : 'a list; their : 'a list; order : origin }

val of_hunk : 'a Patch.hunk -> 'a t list

(* Turns a hunk into a list of blocks by grouping consecutive changed lines. *)
val to_hunk : 'a t list -> 'a Patch.hunk
(* Turns back a list of blocks into a hunk.
       [x |> of_hunk |> to_hunk] should yield [x] back. *)
