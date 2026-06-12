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
  | Flat  of string (* on met les space deant et le cas vide *)
  | Var   of string
  | Subsh of expr
  
and dquotwords = dquotword list

