module State :
sig
  (* type représentant un state *)

  (* permet de représenter plusieurs types de valeur *)
  type value
  (* Contenu en mémoire, nom variable et valeur associé *)
  type state

  (* fonction utile pour state *)
  val create_state : state
  val init_state : state -> state
  val read_var : string -> state -> int
  val modify_var : string -> 't -> state -> state

  val print_state : state
  
  
end
