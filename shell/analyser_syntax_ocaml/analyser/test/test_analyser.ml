open AnalyseurShell

let () =
  let inp = (And (Command [Flat "cd"; Flat "/"], Command [Flat "ls"])) in 
  print_endline (jsonify inp)