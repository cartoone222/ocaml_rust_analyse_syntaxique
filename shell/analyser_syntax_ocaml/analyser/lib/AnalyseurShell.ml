(* open AnalyseurDeBase *)

type expr =
  | Affect  of string * expr 
  | And     of expr   * expr
  | Andlasy of expr   * expr
  | Or      of expr   * expr
  | Orlasy  of expr   * expr
  | Pipeout of expr   * expr
  | Pipein  of expr   * expr
  | Pipeoutsup of expr * expr
  | Pipeerr of expr   * expr
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

(* _______________________ parseur _______________________ *)



(* _______________________ convertion de json _______________________ *)

let jsontemplat typ op arg = "{\"type\" : \"" ^ typ ^ "\", \"op\" : \"" ^ op ^ "\", \"arg\" : [" ^ arg ^ "]}"

let rec jsonify = function
  | Affect     (nom, expr)    -> jsontemplat "Expr" "Affect"     (nom ^ "," ^ jsonify expr ^ "]")
  | And        (expr1, expr2) -> jsontemplat "Expr" "And"        (jsonify expr1 ^ "," ^ jsonify expr2)
  | Andlasy    (expr1, expr2) -> jsontemplat "Expr" "Andlasy"    (jsonify expr1 ^ "," ^ jsonify expr2)
  | Or         (expr1, expr2) -> jsontemplat "Expr" "Or"         (jsonify expr1 ^ "," ^ jsonify expr2)
  | Orlasy     (expr1, expr2) -> jsontemplat "Expr" "Orlasy"     (jsonify expr1 ^ "," ^ jsonify expr2)
  | Pipein     (expr1, expr2) -> jsontemplat "Expr" "Pipein"     (jsonify expr1 ^ "," ^ jsonify expr2)
  | Pipeout    (expr1, expr2) -> jsontemplat "Expr" "Pipeoutsup" (jsonify expr1 ^ "," ^ jsonify expr2)
  | Pipeoutsup (expr1, expr2) -> jsontemplat "Expr" "Pipeoutsup" (jsonify expr1 ^ "," ^ jsonify expr2)
  | Pipeerr    (expr1, expr2) -> jsontemplat "Expr" "Pipeerr"    (jsonify expr1 ^ "," ^ jsonify expr2)
  | Chain      (expr1, expr2) -> jsontemplat "Expr" "Chain"      (jsonify expr1 ^ "," ^ jsonify expr2)
  | Command    (l)            -> let ml = (List.map (jsonify_aux_arg) l) in 
                                    match ml with
                                      | (x :: xs) -> jsontemplat "Expr" "Command"    (List.fold_right (fun a b ->  b ^ "," ^ a) xs x)
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

(* let parseurjson inp = match parseur inp with
                                  | [] -> ""
                                  | (c, _) :: _ -> jsonify c *)