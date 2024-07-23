module WordDiff : sig
  type word_diff =
    | WDeleted of string array
    | WAdded of string array
    | WEqual of string array

  type hunk = {
    mine_start : int;
    mine_len : int;
    their_start : int;
    their_len : int;
    lines : line_change list;
  }

  and line_change =
    | Common of string
    | Added of string
    | Deleted of string
    | Modified of word_diff list

  val compute : Patch.hunk -> hunk
  val render_hunk : hunk -> Nottui.ui
end
