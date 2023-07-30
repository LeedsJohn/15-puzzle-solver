open! Core

type t = { board : string; n : int; manhattan_distance : int; empty_pos : int }
[@@deriving compare, sexp]

let get_n t = t.n
let get_manhattan_distance t = t.manhattan_distance

let manhattan_distance board n =
  String.to_list board
  |> List.mapi ~f:(fun i c -> (i, Char.to_int c))
  |> List.sum
       (module Int)
       ~f:(fun (pos, tile) ->
         abs ((pos % n) - (tile % n)) + abs ((pos / n) - (tile / n)))

let of_int_list_exn nums =
  let n = ref (-1) in
  for i = 2 to 16 do
    if i * i = List.length nums then n := i
  done;
  if !n = -1 then
    failwith "length of nums must be a perfect square 4 <= length <= 256";
  let board = List.map nums ~f:Char.of_int_exn |> String.of_char_list in
  let empty_pos = String.index_exn board (Char.of_int_exn ((!n * !n) - 1)) in
  { board; n = !n; manhattan_distance = manhattan_distance board !n; empty_pos }

let create n =
  if n > 16 then failwith "n must be <= 16";
  of_int_list_exn (List.range 0 (n * n))

let copy t = t
let is_solved t = t.manhattan_distance = 0

let print t =
  let get_char c =
    let num = Char.to_int c + 1 in
    if num = t.n * t.n then "*" else Int.to_string num
  in
  String.iteri t.board ~f:(fun i c ->
      if i mod t.n = 0 then Stdio.print_endline "";
      printf "%4s" (get_char c));
  Stdio.print_endline ""

let move t swap_index =
  let board =
    String.mapi t.board ~f:(fun i c ->
        if i = swap_index then String.get t.board t.empty_pos
        else if i = t.empty_pos then String.get t.board swap_index
        else c)
  in
  (* OPTIMIZATION: don't recalculate all of manhattan distance,
     just look at how the manhattan distance for the affected cells change. *)
  {
    board;
    n = t.n;
    manhattan_distance = manhattan_distance board t.n;
    empty_pos = swap_index;
  }

let get_next_boards t =
  List.filter
    [ t.empty_pos + 1; t.empty_pos - 1; t.empty_pos + t.n; t.empty_pos - t.n ]
    ~f:(fun i ->
      i >= 0
      && i < t.n * t.n
      && (abs (t.empty_pos - i) = t.n || t.empty_pos / t.n = i / t.n))
  |> List.map ~f:(fun swap -> (move t swap, swap))

let get_shuffled ?(num_moves = 1000) n () =
  (* do random moves instead of randomly permuting a list because
     I'm not sure if every state is solvable. *)
  List.fold (List.range 0 num_moves) ~init:(create n) ~f:(fun board _ ->
      get_next_boards board |> List.map ~f:fst |> List.random_element_exn)

let%expect_test "testing creation (and solver)" =
  let board = create 4 in
  print board;
  printf "is solved: %b manhattan_distance: %d\n" (is_solved board)
    board.manhattan_distance;
  [%expect
    {|
       1   2   3   4
       5   6   7   8
       9  10  11  12
      13  14  15   *
    is solved: true manhattan_distance: 0 |}];

  let board = of_int_list_exn [ 5; 4; 3; 2; 0; 1; 8; 6; 7 ] in
  print board;
  printf "is solved: %b manhattan_distance: %d\n" (is_solved board)
    board.manhattan_distance;
  [%expect
    {|
       6   5   4
       3   1   2
       *   7   8
    is solved: false manhattan_distance: 18 |}]

let%expect_test "testing movement" =
  let board = create 3 in
  get_next_boards board
  |> List.sort ~compare:(fun (b1, _) (b2, _) ->
         b1.manhattan_distance - b2.manhattan_distance)
  |> List.iter ~f:(fun (new_board, _) ->
         print new_board;
         printf "Empty position: %d Manhattan distance: %d\n"
           new_board.empty_pos new_board.manhattan_distance);
  [%expect
    {|
       1   2   3
       4   5   6
       7   *   8
    Empty position: 7 Manhattan distance: 2

       1   2   3
       4   5   *
       7   8   6
    Empty position: 5 Manhattan distance: 2 |}];
  let board = of_int_list_exn [ 3; 0; 1; 6; 8; 4; 2; 7; 5 ] in
  get_next_boards board
  |> List.sort ~compare:(fun (b1, _) (b2, _) ->
         b1.manhattan_distance - b2.manhattan_distance)
  |> List.iter ~f:(fun (new_board, _) ->
         print new_board;
         printf "Empty position: %d Manhattan distance: %d\n"
           new_board.empty_pos new_board.manhattan_distance);
  [%expect
    {|
       4   1   2
       7   5   *
       3   8   6
    Empty position: 5 Manhattan distance: 10

       4   1   2
       7   8   5
       3   *   6
    Empty position: 7 Manhattan distance: 12

       4   1   2
       *   7   5
       3   8   6
    Empty position: 3 Manhattan distance: 14

       4   *   2
       7   1   5
       3   8   6
    Empty position: 1 Manhattan distance: 14 |}]
