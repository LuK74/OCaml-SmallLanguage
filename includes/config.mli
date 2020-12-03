open AnalyseurSyntaxique
open State

module A = AnalyseurSyntaxique
module S = State

module Config :
sig
  (* Type représentant une config *)
  type config

  (* Execution une affectation sur un state donné *)
  val exec_affec : A.instr -> S.state -> S.state
  (* Un pas dans le programme *)
  val one_step : A.instr -> S.state -> config
  (* Lances l'éxecution du programme *)
  val execute : A.instr -> S.state

end
