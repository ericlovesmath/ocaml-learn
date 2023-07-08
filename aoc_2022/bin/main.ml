open Printf

let solve part1 part2 id =
  let file = sprintf "data/day%02d.txt" id in
  printf "[Day %02d]\n  1. %s\n  2. %s\n" id (part1 file) (part2 file)

let () =
  solve Day01.part1 Day01.part2 1;
  solve Day02.part1 Day02.part2 2;
  solve Day03.part1 Day03.part2 3
