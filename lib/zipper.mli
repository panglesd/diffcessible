type 'a zipper = { before : 'a list; current : 'a option; after : 'a list; }
val zipper_of_list : 'a list -> 'a zipper
val test_zipper_of_list : (int list -> int zipper) -> bool
val next_zipper : 'a zipper -> 'a zipper
val test_next_zipper : (int zipper -> int zipper) -> bool
val prev_zipper : 'a zipper -> 'a zipper
val test_prev_zipper : (int zipper -> int zipper) -> bool
