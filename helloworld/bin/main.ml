print_endline "Hello, World!"

let rec factorial n = if n = 1 then 1 else n * factorial (n - 1)
let () = print_int (factorial 5)

(* `and` for shared recursive *)
let rec even n = n = 0 || odd (n - 1)
and odd n = n <> 0 && even (n - 1)

let () = print_string ("\n" ^ string_of_bool (even 5) ^ "\n")

(* Equivalent Statements *)
let inc_1 x = x + 1
let inc_2 = fun x -> x + 1

(* Pipe operations *)
let square x = x * x
let () = print_int (5 |> inc_1 |> square |> inc_2 |> inc_2 |> square)

(* Labeled and Optional Funcs *)
let f ~name1:arg1 ~name2:arg2 = arg1 + arg2;;
let f ~arg1 ~arg2 = arg1 + arg2;;
let f ?name:(arg1 = 8) arg2 = arg1 + arg2

(* Partial Application Equivalence
   All functions are just secretly right-associative *)
let add x y = x + y
let add x = fun y -> x + y
let add = fun x -> (fun y -> x + y)

(* Infix Operatiors *)
let add3 = ( + ) 3
let ( ^^ ) x y = max x y

(* Reusing Stack Frames (Tail Recursion)
   Remove all computations at end of each call stack *)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let rec count_aux n acc = if n = 0 then acc else count_aux (n - 1) (acc + 1)
let count_tr n = count_aux n 0

(** [sum lst] is the sum of the elements of [lst]. *)
let print_stat name num =
  Printf.printf "%s: %i\n%!" name num

