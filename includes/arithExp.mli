open State
open AnalyseurLexicale

module S = State
module AL = AnalyseurLexicale
open State

module ArithExp :
sig
  type aExp
  
  val printAExp : aExp
  val evalAExp : aExp -> int
  
end
