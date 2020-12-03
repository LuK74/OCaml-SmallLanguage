open ArithExp
open State

module S = State
module Ar = ArithExp

module BoolExp :
sig
  (* Type représentant une expression booléenne *)
  type boolExp

  (* Affichage une expression booléenne, pour debug *)
  val printBExp : boolExp
  (* Evalue une expression booléenne *)
  val evalBExp : boolExp -> bool

end
