val for_all : ('a -> bool) -> 'a list -> bool
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val combine : 'a list -> 'b list -> ('a * 'b) list
val map : ('a -> 'b) -> 'a list -> 'b list
val print_int_list : int list -> unit
