open AnalyseurSyntaxique
open State
open AnalyseurLexicale
open ArithExp
open BoolExp

module A = AnalyseurSyntaxique
module Al = AnalyseurLexicale
module S = State
module Ar = ArithExp
module Bo = BoolExp

module Natural :
sig
  
  (* Execution une affectation sur un state donné *)
  val exec_affec : A.instr -> S.state -> S.state
  (* Un pas dans le programme *)
  val one_step : A.instr -> S.state -> S.state
  (* Lances l'éxecution du programme *)
  val execute : A.instr -> S.state

end
