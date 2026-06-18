open AnalyseurShell

let () =
  let inp = (And (Command [Flat "cd"; Flat "/"], Command [Flat "ls"])) in 
  print_endline (jsonify inp);

  match word "test test" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (a ^ "\n" ^ b);

  match p_arg_dquot "\"test test\"" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify_aux_arg a ^ "\n" ^ b);

  match p_arg_dquot "\"test $test\"" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify_aux_arg a ^ "\n" ^ b);

  match p_arg_dquot "\"test $()\"" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify_aux_arg a ^ "\n" ^ b);

  match p_expr_command "test \"test $()\"" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify a ^ "\n" ^ b);
  
  match p_expr_command "test \"test $()\"        " with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify a ^ "\n" ^ "(" ^ b ^ ")");
    
  match p_expr_command "test \"test $()\"        abc" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify a ^ "\n" ^ "(" ^ b ^ ")");

  match p_expr_command "test \"test $()\"        abc" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (jsonify a ^ "\n" ^ "(" ^ b ^ ")");

  match quoted_str "'je suis'" with
    | [] -> ();
    | (a, b) :: _ -> print_endline (a ^ "\n" ^ "(" ^ b ^ ")");
