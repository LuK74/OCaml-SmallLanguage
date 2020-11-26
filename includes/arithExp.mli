open State

module ArithExp :
sig
  type aExpENT
  type aExpFLOAT
  type aExp
  
  val printAExp : aExp
  val evalAExp : aExp -> int
  
end
