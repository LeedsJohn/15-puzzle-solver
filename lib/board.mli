open! Core


type t [@@deriving compare, sexp]

(* board:
     0      1      2   ... n
   n + 1  n + 2  n + 3 ... 2n
   ...
                           n^2 - 1
*)

val get_n : t -> int

val get_manhattan_distance : t -> int

val print : t -> unit

val of_int_list_exn : int list -> t

val create : int -> t 

val copy : t -> t

val is_solved : t -> bool

(* returns boards and swap indices *)
val get_next_boards : t -> (t * int) list

val move : t -> int -> t

val get_shuffled : ?num_moves:int -> int -> unit -> t