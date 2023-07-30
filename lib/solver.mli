open! Core

(* Algorithm:
   1) Create a priority queue of boards sorted by their manhattan distance from
   being solved.
   2) Pop a board from the priority queue
   3) If it's solved, return the sequence of moves leading to that board
   4) For each board in a neighboring state,
      if it has not been visited already, add it to the priority queue
   5) Go to step 2

*)
val solve : Board.t -> int list option