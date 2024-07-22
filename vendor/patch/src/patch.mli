type 'a line = [`Common of 'a  | `Mine of 'a | `Their of 'a ]

type 'a hunk = {
  mine_start : int ;
  mine_len : int ;
  their_start : int ;
  their_len : int ;
  lines : 'a line list ;
}

val mine : 'a hunk -> 'a list
val their : 'a hunk -> 'a list
val pp_hunk : Format.formatter -> string hunk -> unit

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string

val pp_operation : git:bool -> Format.formatter -> operation -> unit

val operation_eq : operation -> operation -> bool

type 'a t = {
  operation : operation ;
  hunks : 'a hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

val pp : git:bool -> Format.formatter -> string t -> unit

val to_diffs : string -> string t list

val patch : string option -> string t -> string option
