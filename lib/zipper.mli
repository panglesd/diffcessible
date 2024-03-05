type 'a t = {
  before : 'a list;
  focus : 'a;
  after : 'a list;
  total_length : int;
  current_index : int;
}

exception Empty_list

val zipper_of_list : 'a list -> 'a t
val next : 'a t -> 'a t
val prev : 'a t -> 'a t
val get_focus : 'a t -> 'a
val get_before : 'a t -> 'a list
val get_after : 'a t -> 'a list
val get_total_length : 'a t -> int
val get_current_index : 'a t -> int
