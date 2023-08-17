open Lib

let _ = Printexc.record_backtrace true

let _ =
  let test = Utils.test "Basic Stack Tests" in

  let s = Stack.init () in
  test (Stack.length s = 0);
  test (Stack.is_empty s);

  let s = s |> Stack.push "a" in
  test (Stack.length s = 1);
  test (not (Stack.is_empty s));
  test (Stack.peek s = "a");

  let s = s |> Stack.push "b" |> Stack.push "c" in
  test (Stack.length s = 3);
  test (Stack.peek s = "c");
  test (Stack.to_string Fun.id s = "[c b a]");

  let s = s |> Stack.pop in
  test (Stack.peek s = "b");
  let s = s |> Stack.pop in
  test (Stack.peek s = "a");

  let s = s |> Stack.pop in
  test (Stack.is_empty s);
  test (Stack.to_string Fun.id s = "[]")

let _ =
  let test _ = Utils.test_throws "Exception Tests" Stack.Empty in

  let s = Stack.init () in
  test () (fun _ -> Stack.peek s);
  test () (fun _ -> Stack.pop s)

let _ = print_endline "Stack Tests Passed"
