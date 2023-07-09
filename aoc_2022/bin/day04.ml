open Core

type interval = { l : int; r : int }

let intervals_of_list = function
  | [ x1; y1; x2; y2 ] -> ({ l = x1; r = y1 }, { l = x2; r = y2 })
  | _ -> failwith "Day04.intervals_of_list: Unexpected Input"

let is_subset intervals =
  let i1, i2 = intervals in
  (i1.l <= i2.l && i2.r <= i1.r) || (i2.l <= i1.l && i1.r <= i2.r)

let is_overlap intervals =
  let i1, i2 = intervals in
  i1.l <= i2.r && i2.l <= i1.r

let parse file =
  let parse_row row =
    row
      |> String.split_on_chars ~on:[ '-'; ',' ]
      |> List.map ~f:Int.of_string
      |> intervals_of_list
  in
  In_channel.read_all file
    |> String.split_lines
    |> List.map ~f:parse_row

let count_intervals cond data =
  data
    |> List.filter ~f:cond
    |> List.length

let part1 file = count_intervals is_subset (parse file) |> string_of_int
let part2 file = count_intervals is_overlap (parse file) |> string_of_int
