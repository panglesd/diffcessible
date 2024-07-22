type word = Unchanged of string | Changed of string
type line_content = word list

val compute : string Block.t -> line_content Block.t
type word_diff =
  | WDeleted of string array
  | WAdded of string array
  | WEqual of string array

type line_change =
  | CommonWord of string
  | AddedWord of string
  | DeletedWord of string
  | ModifiedDiff of word_diff list

type hunk = {
  mine_start : int;
  mine_len : int;
  their_start : int;
  their_len : int;
  lines : line_change list;
}

val compute : Patch.hunk -> hunk
val render_hunk : hunk -> Nottui.ui
