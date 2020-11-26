module BoolExp :
sig
  type boolExp

  val printBExp : boolExp
  val evalBExp : boolExp -> bool

end
