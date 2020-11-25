open AnalyseurSyntaxique
open State

module A = AnalyseurSyntaxique
module S = State

module Config :
sig
  type config

  val one_step : A.instr -> S.state -> config
  val execute : A.instr -> S.state

end
