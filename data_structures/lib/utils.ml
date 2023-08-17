let test msg cond =
  assert (
    if not cond then print_endline ("Test: " ^ msg);
    cond)
