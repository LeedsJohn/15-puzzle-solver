open! Core
open Fifteen_puzzle

(* Average movecount: ~235 *)

let () = print_endline ""

let () =
  let num_trials = 1000 in
  let min', max', total =
    List.fold (List.range 0 num_trials) ~init:(Int.max_value, 0, 0)
      ~f:(fun (min', max', total) _ ->
        let board = Board.get_shuffled 4 () in
        match Solver.solve board with
        | None ->
            print_endline "failed to find solution";
            (min', max', total)
        | Some path ->
            let n = List.length path in
            (min min' n, max max' n, total + n))
  in
  (* TODO: figure out how to make these left align*)
  printf "min  max  avg \n%4d %4d %4d\n" min' max' (total / num_trials)
