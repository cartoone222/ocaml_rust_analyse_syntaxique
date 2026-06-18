open AnalyseurDeBase

type expr =
  | Parentised of expr
  | AffectSsh  of string * expr
  | AffectStr  of string * string
  | AffectVar  of string * string
  | AffectDq   of string * dquotwords
  | And     of expr   * expr
  | Andlasy of expr   * expr
  | Or      of expr   * expr
  | Orlasy  of expr   * expr
  | Pipeout of expr   * string
  | Pipein  of expr   * string
  | Pipeoutsup of expr * string
  | Pipeerr of expr   * string
  | Chain   of expr   * expr
  | Command of arg list

and arg = 
  | Flat  of string
  | Quot  of string
  | Var   of string
  | Subsh of expr (* $(expression) *)
  | Dquot of dquotwords
  
and dquotword =
  | Flatqd  of string (* on met les space deant et le cas vide *)
  | Varqd   of string
  | Subshdq of expr
  
and dquotwords = dquotword list

(* _______________________ utils parseur _______________________ *)

let digit = sat (fun c -> List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
let lettre       = sat (fun c -> List.mem c ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'])
let upperlettre  = sat (fun c -> List.mem c ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'])
let lightspecial = sat (fun c -> List.mem c ['/'; '.'])
let special      = sat (fun c -> List.mem c ['!'; '\\'; '#'; '$'; '%'; '&'; '\''; '('; ')'; '*'; '+'; ','; '-'; '.'; '/'; ':'; ';'; '<'; '='; '>'; '?'; '@'; '['; ']'; '^'; '_'; '`'; '{'; '|'; '}'; '~'])
let space = char ' '
let spaces = many space
let underscore = char '_'
let midelscore = char '-'

let alphanum = digit <|> lettre <|> upperlettre

let light_word = let>>= cs = some (alphanum <|> underscore <|> midelscore <|> lightspecial) in
           pure (String.of_seq (List.to_seq cs))

let word = let>>= cs = some (alphanum <|> special) in
           pure (String.of_seq (List.to_seq cs))

let var = char '$' >> let>>= name = light_word in pure name

let parentised p =
  char '(' >>
  let>>= v = p in
  char ')' >>
  pure v

let quoted_str = char '\'' >> 
                 let>>= c = many (word <|> string " ") in 
                 char '\'' >> 
                 pure (String.concat "" c)

(* _______________________ main parseur _______________________ *)

let p_dquotword_flat  = let>>= fl = word  in pure (Flatqd fl)
let p_dquotword_var   = let>>= vn = var   in pure (Varqd vn)

let p_arg_flat        = let>>= w = light_word in pure (Flat w)
let p_arg_quot        = let>>= str = quoted_str in pure (Quot (str))
and p_arg_var         = let>>= vn = var in pure (Var vn)

let op_logique = (spaces >> string "&"  >> spaces >> pure (fun a b -> And     (a,b))) <|>
                 (spaces >> string "&&" >> spaces >> pure (fun a b -> Andlasy (a,b))) <|>
                 (spaces >> string "|"  >> spaces >> pure (fun a b -> Or      (a,b))) <|>
                 (spaces >> string "||" >> spaces >> pure (fun a b -> Orlasy  (a,b)))

let op_chain = (spaces >> char ';' >> spaces >> pure (fun a b -> Chain (a, b)))

let rec subsh inp = (string "$(" >>
            let>>= expr = p_expr in
            char ')' >> 
            pure expr) inp

and p_dquotword_subsh inp = (let>>= sh = subsh in pure (Subshdq sh)) inp
and p_dquotword inp = (p_dquotword_subsh <|> p_dquotword_var <|> p_dquotword_flat) inp

and p_arg_dquot inp = (char '"' >>
                  let>>= l = manysep p_dquotword spaces in
                  char '"' >>
                  pure (Dquot l)) inp
and p_arg_subsh inp = (let>>= sh = subsh in pure (Subsh sh)) inp
and p_arg inp = (choices [p_arg_subsh; p_arg_var; p_arg_quot; p_arg_dquot; p_arg_flat]) inp

and p_expr_parentised inp = (parentised p_expr) inp
and p_expr_command inp = (let>>= l = somesep p_arg spaces in pure (Command l)) inp

and p_expr_affectSsh inp = (let>>= name = light_word in
                            char '=' >>
                            let>>= exp = subsh in
                            pure (AffectSsh (name, exp))) inp
and p_expr_affectStr inp = (let>>= name = light_word in
                            char '=' >>
                            let>>= str = quoted_str in
                            pure (AffectStr (name, str))) inp
and p_expr_affectVar inp = (let>>= name = light_word in
                            char '=' >>
                            let>>= var = var in
                            pure (AffectVar (name, var))) inp
and p_expr_affectDq  inp = ((let>>= name = light_word in
                            char '=' >>
                            let>>= l = manysep p_dquotword spaces in
                            pure (AffectDq (name, l)))) inp

and op_pipe = (spaces >> char   '>'  >> spaces >> pure (fun a b -> Pipeout    (a, b))) <|> (* casser a fix c'est pas bon *)
              (spaces >> string "<"  >> spaces >> pure (fun a b -> Pipein     (a, b))) <|> (* casser a fix c'est pas bon *)
              (spaces >> string ">>" >> spaces >> pure (fun a b -> Pipeoutsup (a, b))) <|> (* casser a fix c'est pas bon *)
              (spaces >> string "2>" >> spaces >> pure (fun a b -> Pipeerr    (a, b)))     (* casser a fix c'est pas bon *)

and p_expr_chaine  input = (chainl1 p_expr_pipe op_chain   ) input
and p_expr_pipe    input = (p_expr_logique) input (* TODO pipe clause *)
and p_expr_logique input = (chainl1 p_expr_atome op_logique) input
and p_expr_atome   input = (p_expr_parentised <|> 
                            p_expr_affectSsh <|> 
                            p_expr_affectStr <|> 
                            p_expr_affectVar <|> 
                            p_expr_affectDq  <|> 
                            p_expr_command) input

and p_expr inp = (p_expr_chaine) inp

(* _______________________ convertion de json _______________________ *)

let jsontemplat typ op arg = "{\"type\" : \"" ^ typ ^ "\", \"op\" : \"" ^ op ^ "\", \"arg\" : [" ^ arg ^ "]}"

let rec jsonify = function
  | Parentised (expr)         -> jsontemplat "Expr" "Parentised" (jsonify expr)
  | AffectSsh  (nom, expr)    -> jsontemplat "Expr" "AffectSsh"  (nom ^ "," ^ jsonify expr)
  | AffectStr  (nom, str)     -> jsontemplat "Expr" "AffectStr"  (nom ^ "," ^ str)
  | AffectVar  (nom, v)       -> jsontemplat "Expr" "AffectVar"  (nom ^ "," ^ v)
  | AffectDq   (nom, l)       -> (let ml = (List.map (jsonify_aux_dquot) l) in 
                                   match ml with
                                     | (x :: xs) -> jsontemplat "Expr" "AffectDq" (nom ^ "," ^ (List.fold_right (fun a b ->  b ^ "," ^ a) xs x))
                                     | []        -> jsontemplat "Expr" "AffectDq" "nom") (* warning *)
  | And        (expr1, expr2) -> jsontemplat "Expr" "And"        (jsonify expr1 ^ "," ^ jsonify expr2)
  | Andlasy    (expr1, expr2) -> jsontemplat "Expr" "Andlasy"    (jsonify expr1 ^ "," ^ jsonify expr2)
  | Or         (expr1, expr2) -> jsontemplat "Expr" "Or"         (jsonify expr1 ^ "," ^ jsonify expr2)
  | Orlasy     (expr1, expr2) -> jsontemplat "Expr" "Orlasy"     (jsonify expr1 ^ "," ^ jsonify expr2)
  | Pipein     (expr1, str)   -> jsontemplat "Expr" "Pipein"     (jsonify expr1 ^ "," ^ str)
  | Pipeout    (expr1, str)   -> jsontemplat "Expr" "Pipeoutsup" (jsonify expr1 ^ "," ^ str)
  | Pipeoutsup (expr1, str)   -> jsontemplat "Expr" "Pipeoutsup" (jsonify expr1 ^ "," ^ str)
  | Pipeerr    (expr1, str)   -> jsontemplat "Expr" "Pipeerr"    (jsonify expr1 ^ "," ^ str)
  | Chain      (expr1, expr2) -> jsontemplat "Expr" "Chain"      (jsonify expr1 ^ "," ^ jsonify expr2)
  | Command    (l)            -> let ml = (List.map (jsonify_aux_arg) l) in 
                                    match ml with
                                      | (x :: xs) -> jsontemplat "Expr" "Command" (List.fold_left (fun a b ->  a ^ "," ^ b) x xs)
                                      | []        -> jsontemplat "Expr" "Command" ""

and jsonify_aux_arg = function
  | Flat       (str)          -> jsontemplat "Arg" "Flat"        ("\"" ^ str ^ "\"")
  | Quot       (str)          -> jsontemplat "Arg" "Quot"        ("\"" ^ str ^ "\"")
  | Var        (str)          -> jsontemplat "Arg" "Var"         ("\"" ^ str ^ "\"")
  | Subsh      (expr)         -> jsontemplat "Arg" "Subsh"       (jsonify expr)
  | Dquot      (l)            -> let ml = (List.map (jsonify_aux_dquot) l) in 
                                    match ml with
                                      | (x :: xs) -> jsontemplat "Arg" "Dquot" (List.fold_right (fun a b ->  b ^ "," ^ a) xs x)
                                      | []        -> jsontemplat "Arg" "Dquot" ""

and jsonify_aux_dquot = function
  | Flatqd     (str)          -> jsontemplat "Dquotword" "Flatqd"  ("\"" ^ str ^ "\"")
  | Varqd      (str)          -> jsontemplat "Dquotword" "Varqd"   ("\"" ^ str ^ "\"")
  | Subshdq    (expr)         -> jsontemplat "Dquotword" "Subshdq" (jsonify expr)

let parseurjson inp = match p_expr inp with
                        | [] -> ""
                        | (c, _) :: _ -> jsonify c
