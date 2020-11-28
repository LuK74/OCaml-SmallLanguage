open ArithExp
open State

module S = State
module Ar = ArithExp

module BoolExp :
sig
  type boolExp

  val printBExp : boolExp
  val evalBExp : boolExp -> bool

end
