open! Core

module Board_map = struct
  module T = struct
    type t = Board.t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make_plain (T)
end

module Board_set = Set.Make (struct
  type nonrec t = Board.t [@@deriving compare, sexp]
end)

let solve (board : Board.t) =
  let n = Board.get_n board in
  (* rough estimate for maximum manhattan distance a board could be in
     (furthest distance one tile could be from where it's supposed to be
     times the number of tiles) *)
  let distances =
    Array.init
      (n * n * ((2 * n) - 2))
      ~f:(fun _ -> Map.empty (module Board_map))
  in
  (* this should really be something mutable instead of a ref *)
  let visited = ref Board_set.empty in
  (* TODO: if it's already in distances but it's path is shorter, replace it *)
  let add_to_distances b path =
    if Set.mem !visited b then ()
    else
      let distance = Board.get_manhattan_distance b in
      match Map.add distances.(distance) ~key:b ~data:path with
      | `Duplicate -> ()
      | `Ok map -> distances.(distance) <- map
  in
  add_to_distances board [];
  let get_closest () =
    match Array.find distances ~f:(fun map -> not (Map.is_empty map)) with
    | None -> None
    | Some map ->
        (* is there an O(1) way to do this instead of O(log(n))? *)
        let board, path = Map.nth_exn map 0 in
        let distance = Board.get_manhattan_distance board in
        distances.(distance) <- Map.remove distances.(distance) board;
        visited := Set.add !visited board;
        Some (board, path)
  in
  let rec aux cur_board path =
    if Board.is_solved cur_board then Some (List.rev path)
    else (
      List.iter (Board.get_next_boards cur_board) ~f:(fun (b, swap) ->
          add_to_distances b (swap :: path));
      match get_closest () with
      | None -> None
      | Some (board, path) -> aux board path)
  in
  aux board []

let%expect_test "verify solver" =
  let board = Board.get_shuffled 4 () in
  solve board |> Option.value_exn
  |> List.fold ~init:board ~f:(fun acc swap -> Board.move acc swap)
  |> Board.print;
  [%expect
    {|
     1   2   3   4
     5   6   7   8
     9  10  11  12
    13  14  15   * |}]

(* let add_to_distances b path =
     if Set.mem !visited b then ()
     else
       let distance = Board.get_manhattan_distance b in
       match Map.find distances.(distance) b with
       | None ->
           distances.(distance) <-
             Map.add_exn distances.(distance) ~key:b ~data:path
       | Some old_path ->
           if List.length old_path <= List.length path then ()
           else
             distances.(distance) <-
               Map.update distances.(distance) b ~f:(fun _ -> path)
   in *)
