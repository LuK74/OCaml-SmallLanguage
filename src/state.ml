module State =
  struct
    (* 'c identifier, 'a : value, state : suite du state *)
    type value =
      | VEnt of int
      | VFloat of float
      | VBool of bool
    
    type state =
      | EOS
      | State of (string * value * state)

    let create_state =
      EOS

    let rec init_state = fun s ->
      match s with
      | State(id, value, suite) -> State(id, VEnt(0), (init_state suite))
      | EOS -> s

    exception VarNotFound of string * state
    
    let rec read_var = fun id ->
      fun s ->
      match s with
      | State(id0, value, suite) -> if (id0 = id) then value
                                    else read_var id suite
      | EOS -> raise (VarNotFound (id,s))

    let rec modify_var = fun id ->
      fun newVal ->
      fun s ->
      match s with
      | State(id0, value, suite) -> if (id0 = id) then State(id0,newVal,suite)
                                    else State(id0, value, (modify_var id newVal suite))
      | EOS -> State(id, newVal, EOS)

    exception ComputeHashWentWrong
    let computeHash = fun value ->
      match value with
      | 0 -> 1
      | 1 -> 0
      | _ -> raise ComputeHashWentWrong

        exception InstrNotAssign

        let print_value = fun v ->
          match v with
          | VEnt(k) -> print_int k
          | VFloat(k) -> print_float k
          | VBool(k) -> if (k = true) then print_string "true" else print_string "false"
        
    let rec print_state = fun s ->
      match s with
      | State(id0, value, suite) ->
         print_string id0; print_string " : "; print_value value;
         print_string " | "; print_state suite
      | EOS -> print_string " End Of State \n"; s
   

end

