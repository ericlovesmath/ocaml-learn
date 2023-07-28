open Core

type move = Rock | Paper | Scissors
type result = Win | Draw | Lose

let get_points move result =
  let move_points = match move with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let result_points = match result with Lose -> 0 | Draw -> 3 | Win -> 6 in
  move_points + result_points

let parse file =
  In_channel.read_all file
  |> String.split_lines
  |> List.map ~f:(fun str -> (String.get str 0, String.get str 2))

let sum_int_list = List.fold ~init:0 ~f:( + )

let part1 file =
  let get_result move1 move2 =
    match (move1, move2) with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Win
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Draw
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Lose
  in
  let of_string = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | _ -> failwith "Day02.part1: of_string given invalid input"
  in
  parse file
  |> List.map ~f:(fun (opp, me) -> (of_string opp, of_string me))
  |> List.map ~f:(fun (opp, me) -> get_points me (get_result me opp))
  |> sum_int_list
  |> string_of_int

let part2 file =
  let get_move move result =
    match (move, result) with
    | Rock, Draw | Paper, Lose | Scissors, Win -> Rock
    | Rock, Win | Paper, Draw | Scissors, Lose -> Paper
    | Rock, Lose | Paper, Win | Scissors, Draw -> Scissors
  in
  let parse_move = function
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | _ -> failwith "Day02.part2: parse_move given invalid input"
  in
  let parse_result = function
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> failwith "Day02.part2: parse_result given invalid input"
  in
  parse file
  |> List.map ~f:(fun (move, result) -> (parse_move move, parse_result result))
  |> List.map ~f:(fun (move, result) ->
       get_points (get_move move result) result)
  |> sum_int_list
  |> string_of_int
