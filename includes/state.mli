module State :
sig
  (* type représentant un state *)
  type value
  type state

  (* fonction utile pour state *)
  val create_state : state
  val init_state : state -> state
  val read_var : string -> state -> int
  val modify_var : string -> 't -> state -> state

  val print_state : state
  
  
end
