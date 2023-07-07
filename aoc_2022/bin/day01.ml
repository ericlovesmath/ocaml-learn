open Core
open Stdio

let sum list = List.fold_left list ~init:0 ~f:( + )

let parse file =
  let int_list_of_string list =
    List.map ~f:Int.of_string (String.split_lines list)
  in
  In_channel.read_all file
    |> Str.split (Str.regexp "\n\n")
    |> List.map ~f:int_list_of_string
    |> List.map ~f:sum

let part1 file =
  let max list = List.fold_left list ~init:0 ~f:max in
  parse file |> max |> string_of_int

let part2 file =
  let rec last_three = function
    | [ _; _; _ ] as ans -> ans
    | _ :: rem -> last_three rem
    | _ -> failwith "Less than three elements"
  in
  parse file
    |> List.sort ~compare:Int.compare
    |> last_three
    |> sum
    |> string_of_int
