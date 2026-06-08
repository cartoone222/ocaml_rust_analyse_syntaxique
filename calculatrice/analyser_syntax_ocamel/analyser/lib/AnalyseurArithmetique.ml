open AnalyseurDeBase

type expression = 
  | Val of int
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression

let space =  char ' '

let spaces = many space

let digit = sat (fun c -> List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])

let digits = some digit

let int = 
  let>>= dig = digits in
  let toInt = fun x -> int_of_string (String.of_seq (List.to_seq x)) in
  pure (toInt dig)

let parentised p =
  char '(' >>
  let>>= v = p in
  char ')' >>
  pure v

(* definition des partseur de gramaire *)

(* gramaire a inplemme
somme est un enseble de facteur intrecaler de + et -
facteur est un exseble d'exprestion intrecaler de * et /
expresion valeur ou entre parenthese somme *)

(* les parsuer operateur sont des parsuer qui consome du texte et retourn une fonction *)

let opSomme = (char '+' >> pure (fun a b -> Add (a, b))) <|>
              (char '-' >> pure (fun a b -> Sub (a, b)))

let opFacteur = (char '*' >> pure (fun a b -> Mul (a, b))) <|>
                (char '/' >> pure (fun a b -> Div (a, b)))

let expr_val = let>>= v = int in (pure (Val v))

let rec somme   input = (chainl1 facteur opSomme       ) input
    and facteur input = (chainl1 expr    opFacteur     ) input
    and expr    input = (parentised somme <|> expr_val ) input

let parseur = somme

let rec pp = function
  | Val v        -> "(Val " ^ string_of_int v ^ ")"
  | Add (x1, x2) -> "(Add " ^ pp x1 ^ " " ^ pp x2 ^ ")"
  | Sub (x1, x2) -> "(Sub " ^ pp x1 ^ " " ^ pp x2 ^ ")"
  | Mul (x1, x2) -> "(Mul " ^ pp x1 ^ " " ^ pp x2 ^ ")"
  | Div (x1, x2) -> "(Div " ^ pp x1 ^ " " ^ pp x2 ^ ")"

let jsonop2 op2 arg1 arg2 = "{\"op\" : \"" ^ op2 ^ "\", \"arg\" : [" ^ arg1 ^ ", " ^ arg2 ^ "]}"

let rec jsonify = function
  | Val v        -> string_of_int v
  | Add (x1, x2) -> jsonop2 "add" (jsonify x1) (jsonify x2)
  | Sub (x1, x2) -> jsonop2 "sub" (jsonify x1) (jsonify x2)
  | Mul (x1, x2) -> jsonop2 "mul" (jsonify x1) (jsonify x2)
  | Div (x1, x2) -> jsonop2 "div" (jsonify x1) (jsonify x2)

let parseurjson inp = match parseur inp with
                                  | [] -> ""
                                  | (c, _) :: _ -> jsonify c

let () = Callback.register "parseurjson" parseurjson