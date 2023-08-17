open Lib

let () =
  let test = Utils.test "Basic Stack Tests" in

  let stack = Stack.init () in
  test (Stack.length stack = 0);
  test (Stack.is_empty stack);

  let stack = stack |> Stack.push "a" in
  test (Stack.length stack = 1);
  test (not (Stack.is_empty stack));
  test (Stack.peek stack = "a");

  let stack = stack |> Stack.push "b" |> Stack.push "c" in
  test (Stack.length stack = 3);
  test (Stack.peek stack = "c");
  test (Stack.to_string Fun.id stack = "[c b a]");

  let stack = stack |> Stack.pop in
  test (Stack.peek stack = "b");
  let stack = stack |> Stack.pop in
  test (Stack.peek stack = "a");
  let stack = stack |> Stack.pop in
  test (Stack.is_empty stack);
  test (Stack.to_string Fun.id stack = "[]")

let () =
  let test = Utils.test "Exception Tests" in

let () = print_endline "Stack Tests Passed"
