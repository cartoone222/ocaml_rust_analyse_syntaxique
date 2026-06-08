(* let readFile fileName = In_channel.with_open_text fileName In_channel.input_all *)

let input = read_line ()

let () = match AnalyseurArithmetique.parseur input with
  | (c, "") :: _ -> Printf.printf "%s\n" (AnalyseurArithmetique.jsonify c)
  | _ -> print_endline "{\"err\" : \"error not implement\" }"

(* let () = print_endline input *)
