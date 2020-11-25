open AnalyseurSyntaxique

module A = AnalyseurSyntaxique

module State =
  struct
    (* 'c identifier, 'a : value, state : suite du state *)
        
    type state =
      | EOS
      | State of (char * int * state)

    let create_state =
      State('a', 0, State('b',0,State('c',0,State('d',0,EOS))))

    let rec init_state = fun s ->
      match s with
      | State(id, value, suite) -> State(id, 0, (init_state suite))
      | EOS -> s

    exception VarNotFound
    
    let rec read_var = fun id ->
      fun s ->
      match s with
      | State(id0, value, suite) -> if (id0 = id) then value else read_var id suite
      | EOS -> raise VarNotFound

    let rec modify_var = fun id ->
      fun newVal ->
      fun s ->
      match s with
      | State(id0, value, suite) -> if (id0 = id) then State(id0,newVal,suite)  else modify_var id newVal suite
      | EOS -> raise VarNotFound

    exception ComputeHashWentWrong
    let computeHash = fun value ->
      match value with
      | 0 -> 1
      | 1 -> 0
      | _ -> raise ComputeHashWentWrong

        exception InstrNotAssign
    
    let exec_affec = fun instr ->
      fun s ->
      match instr with
      | A.Assign(c,ope) -> (match ope with
                                 | Hash -> modify_var c ((computeHash) (read_var c s)) s
                                 | Exp(A.Var(k)) -> modify_var c (read_var k s) s
                                 | Exp(A.Const(k)) -> modify_var c k s)
      | _ -> raise InstrNotAssign
      
   

end

