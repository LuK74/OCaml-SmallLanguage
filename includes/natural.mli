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

  val exec_affec : A.instr -> S.state -> S.state

  val one_step : A.instr -> S.state -> S.state

  val execute : A.instr -> S.state


end
