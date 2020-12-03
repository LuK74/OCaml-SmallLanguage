open State
open AnalyseurLexicale

module S = State
module AL = AnalyseurLexicale
open State

module ArithExp :
sig
  
  (* Expression arithmétique *)
  type aExp

  (* Affiche une expression arithmétique *)
  val printAExp : aExp

  (* Evalue une expression arithmétique *)
  val evalAExp : aExp -> int
  
end
