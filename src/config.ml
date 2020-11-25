open AnalyseurSyntaxique
open State

module A = AnalyseurSyntaxique
module S = State

module Config =
  struct
    type config = Config of (A.instr * S.state)

    let evalB = fun expB -> fun s ->
      match expB with
      | A.Var(k) -> if ((S.read_var k s) = 0) then false else true
      | A.Const(n) -> if (n = 0) then false else true


    exception StepError of S.state
    
    let rec one_step = fun i1 ->
      fun s1 ->
      try (
      match i1 with
      | A.Skip -> Config(A.Skip, s1)
      | A.Assign(id, ope) -> Config(A.Skip , S.exec_affec i1 s1)
      | A.Seq(i1,i2) -> let Config(instr, s2) = one_step i1 s1 in
                        (match instr with
                         | Skip -> Config(i2, s2)
                         | i1' -> Config(A.Seq(i1',i2), s2))
      | A.While(expB, i1) -> let newInstr = A.If(expB, A.Seq(i1, A.While(expB, i1)), A.Skip) in
                             Config(newInstr, s1)
      | A.If(expB,i1,i2) -> let resultB = evalB expB s1 in
                            if (resultB = true) then Config(i1, s1)
                            else Config(i2,s1))
    with S.VarNotFound(ch, st) -> print_string "Looking for "; print_char ch; print_string " on state : "; let res = S.print_state st in (raise (StepError res))

    let rec execute_aux = fun i ->
      fun s ->
      let Config(newInstr, newState) = one_step i s in
      (match newInstr with
      | Skip -> newState
      | _ -> execute_aux newInstr newState)
    
    let execute = fun instrInit ->
      execute_aux instrInit (S.create_state)
                                   

end

module C = Config

  
let _ = print_string "Parser starts\n"
let _ = print_string "ch1 \n"
let ch1 = A.list_of_string "a:=1;b:=1;c:=1;while(a){if(c){c:=0;a:=b}{b:=0;c:=a}}"
(* a = 0, b = 0, c = 0 *)
let res1 = A.ast_parser_func ch1
let res1 = C.execute res1
let res1 = S.print_state res1

(* met toutes les var à 1 *)
let _ = print_string "ch2 \n"
let ch2 = A.list_of_string "a:=1;b:=1;c:=1;d:=1"
let res2 = A.ast_parser_func ch2
let res2 = C.execute res2
let res2 = S.print_state res2

(* passes tout à 0 (*test des while *) *)
let _ = print_string "ch3 \n"
let ch3 = A.list_of_string"while(a){a:=0;while(b){b:=0;while(c){c:=0;while(d){d:=0}}}}"
let res3 = A.ast_parser_func ch3
let res3 = C.execute res3
let res3 = S.print_state res3

(* si a = 1, c = 0 sinon c = 1, et si b = 1, d = 1 sinon d = 0*)
let _ = print_string "ch4 \n"
let ch4 = A.list_of_string"if(a){c:=0}{c:=1};if(b){d:=1}{d:=0}"
let res4 = A.ast_parser_func ch4
let res4 = C.execute res4
let res4 = S.print_state res4

