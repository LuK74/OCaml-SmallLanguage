open AnalyseurSyntaxique

module A = AnalyseurSyntaxique

module State :
sig
  (* type représentant un state *)
  type id
  type state

  (* fonction utile pour state *)
  val create_state : state
  val init_state : state -> state
  val read_var : char -> state -> int
  val modify_var : char -> int -> state -> state
  val exec_affec : A.instr -> state -> state

  val print_state : state
  
  
end
