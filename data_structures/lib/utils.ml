let test msg cond = if not cond then failwith msg

let test_throws msg ex f =
  match f () with
  | exception e when e == ex -> ()
  | exception e -> failwith (msg ^ ", Unexpected Error" ^ Printexc.to_string e)
  | _ -> failwith msg
