let input = read_line ()

let () = match AnalyseurShell.p_expr input with
  | (c, "") :: _ -> Printf.printf "%s\n" (AnalyseurShell.jsonify c)
  | (c, a) :: _ -> Printf.printf "oups il y as un rest %s\n %s" (AnalyseurShell.jsonify c) a
  | _ -> print_endline "{\"err\" : \"error not implement\" }"