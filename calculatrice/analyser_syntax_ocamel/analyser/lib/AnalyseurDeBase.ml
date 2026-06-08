(* Copyright (C) 2025  Cartoone
  SPDX-License-Identifier: GPL-3.0-or-later 

Code de Cartoone contacte cartoone_222[@]proton.me librement inspirer de l'article :
Hutton, G., & Meijer, E. (n.d.). Monadic parser combinators. Université de Nottingham et Université d'Utrecht

ce code est un projet personelle faisent suit a un cours sur le paseur mondique aprocher de manier experimental que j'ai suivie en L1
il est donc d'un qualiter etudiant et proablement buger reutilisation a vos risque et perile. *)

(* definition type parser *)
type 'a parser = string -> ('a * string) list

(* fonction utilie non dans ocaml *)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

(* LES PARSEUR PRIMITIFS [section 2.1] *)

(* ce parseur reussi tout le temps retournat sont argument sans rien consommer
nom dans l'article : result *)
let pure a =
  fun inp -> [ (a, inp) ]

(* ce parseur echoue tout le temps
nom dans l'article : zero *)
let empty =
  fun _inp -> []

(* ce parseur consome un caracter et le retourn
nom dans l'article : item *)
let item : char parser = function
  | "" -> []
  | s -> [(s.[0], String.sub s 1 (String.length s - 1))]

(* LES PARSEUR DE COMBINAISON [section 2.2] *)

(* ce parsuer permet d'enchainer deux parsuer le resultat du premeir est passer en argument au deuxiemme
nom dans l'article : bind *)
let ( >>= ) p1 pp2 = 
  fun inp -> match p1 inp with
    | [] -> []
    | l ->  List.concat (List.map (uncurry pp2) l)

(* alias pour utiliser la syntaxe let>>= *)
let (let>>=) p1 p2 = p1 >>= p2

(* ce parseur est un equivalent de f >>= k.g ou k est la fonction constent *)
let ( >> ) p1 p2 = p1 >>= Fun.const p2
(* alias pour utiliser la syntaxe let>>= *)
let (let>>) p1 p2 = p1 >> p2

(* ce parseur consome un caracter et reussis s'il verifie bien une assertion
nom dans l'article : sat *)
let sat p : char parser = 
  let>>= c = item in 
    match p c with 
    | true  -> pure c
    | false -> empty

(* ce parseur consome un caracter et reussis s'il est egal a sont argument
nom dans l'article : char *)
let char c : char parser = sat (fun x -> c == x)

(* ce parsuer test deux parseur passer en argument et retourn la concatnation de leur resutat
nom dans l'article : plus *)

let plus p1 p2 = fun inp -> p1 inp @ p2 inp

(* parsuer concatenation de chois avec prioriter sur le premier *)
let ( <|> ) p1 p2 = 
  fun inp -> match p1 inp with
    | [] -> p2 inp
    | result -> result

(* generalisation a une liste de parseur *)
let choices t =
  List.fold_right (fun p acc -> p <|> acc) t (empty)

(* ce parseur consome un mot donner en argument 
nom dans l'article : string *)
let string str : string parser = String.fold_right (fun c acc -> char c >> acc) str (pure str)

(* ces deux parseur repete restpctiment 0 ou n fois et 1 ou n fois le parseur passer en argument
nom dans l'article : many et many1 *)
let rec many p =
  some p <|> pure []
and some p =
  let>>= x = p in
  let>>= xs = many p in
  pure (x :: xs)

(* ce parseur est un somme a la differance qu'un parseur psep doit reussire entre chaque aplication
du paseur passer en argument
nom dans l'article : sepby1 *)
let somesep p psep = 
  let>>= x  = p in 
  let>>= xs = many (psep >> p) in
  pure (x :: xs)

(* pendant many du parsuer precedant
nom dans l'article : sepby *)
let manysep p psep =
  somesep p psep <|> pure []

(* Parse une expression de la forme : p (op p)*
en appliquant les opérateurs de manière associative à gauche.
nom dans l'article : chainl1 *)
let chainl1 p pop =
  let>>= x = p in
  let>>= fys = many (
        let>>= f = pop in
        let>>= y = p   in
        pure (f,y)
      ) in
  pure (List.fold_left (fun a (fu, b) -> fu a b) x fys)
