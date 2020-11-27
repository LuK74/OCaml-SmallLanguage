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

module Config =
  struct
    type config = Config of (A.instr * S.state)

    exception InstrNotAssign    
    let exec_affec = fun instr ->
      fun s ->
      match instr with
      | A.Assign(c,ope) -> (match ope with
                                 | A.Hash -> print_string "Not yet implemented !\n"; s
                                 | A.AExp(ae1) -> (let res = (Ar.evalAExp ae1 s) in
                                                  let newVal = (S.VEnt(res)) in
                                                  (S.modify_var c newVal s))
                                 | A.BExp(be1) -> (let res = (Bo.evalBExp be1 s) in
                                                   let newVal = (S.VBool(res)) in
                                                   S.modify_var c newVal s)
                                 | A.Var(v) -> S.modify_var c (S.read_var v s) s)
      | _ -> raise InstrNotAssign



    exception StepError of S.state
    
    let rec one_step = fun i1 ->
      fun s1 ->
      try (
      match i1 with
      | A.Skip -> Config(A.Skip, s1)
      | A.Assign(id, ope) -> Config(A.Skip , exec_affec i1 s1)
      | A.Seq(i1,i2) -> let Config(instr, s2) = one_step i1 s1 in
                        (match instr with
                         | Skip -> Config(i2, s2)
                         | i1' -> Config(A.Seq(i1',i2), s2))
      | A.While(expB, i1) -> let newInstr = A.If(expB, A.Seq(i1, A.While(expB, i1)), A.Skip) in
                             Config(newInstr, s1)
      | A.If(expB,i1,i2) -> (match expB with
                            | A.BExp(bexp) -> (let resultB = Bo.evalBExp bexp s1 in
                                               (if (resultB = true) then Config(i1, s1)
                                               else Config(i2,s1)))
                            | A.AExp(aexp) -> (let resultA = Ar.evalAExp aexp s1 in
                                               (if (resultA = 0) then Config(i2,s1)
                                                else Config(i1,s1)))
                            | A.Var(v) -> (let resEval = S.read_var v s1 in
                                           match resEval with
                                           | VBool(k) -> if (k = true) then Config(i1,s1) else Config(i2,s1)
                                           | VEnt(k) -> if (k = 0) then Config(i2, s1) else Config(i1, s1)
                                           | _ -> raise (StepError s1))
                            | _ -> raise (StepError s1)))
    with S.VarNotFound(ch, st) -> print_string "Looking for "; print_string ch; print_string " on state : "; let res = S.print_state st in (raise (StepError res))

    let rec execute_aux = fun i ->
      fun s ->
      fun flag ->
      if (flag = 1) then
        (A.print_instr i;
        print_string "(debug) ";
         let user_input = read_line() in
         if (String.equal user_input "continue") then execute_aux i s 0
         else (if (String.equal user_input "print") then (let _ = S.print_state s in execute_aux i s flag)
         else (if (String.equal user_input "next") then  
         (let Config(newInstr, newState) = one_step i s in
          (match newInstr with
           | Skip -> newState
           | _ -> execute_aux newInstr newState flag))
               else execute_aux i s flag)))
    else (let Config(newInstr, newState) = one_step i s in
          (match newInstr with
           | Skip -> newState
           | _ -> execute_aux newInstr newState flag))
    
    let execute = fun instrInit ->
      print_string "Would you like to use debugger ? (Y/N) \n";
      let user_input = read_line () in
      let flag = if (String.equal user_input "Y") then 1 else 0 in
      execute_aux instrInit (S.create_state) flag
                                   

end

module C = Config

let automatedTest = fun s ->
  print_string "Execution starts \n";
  print_string "Prog : \n";
  print_string s;
  print_string "\n";
  print_string "\n";
  let ch1 = Al.list_of_string s in
  let to1 = Al.token_generator ch1 in
  Al.print_token_list to1;
  let res1 = A.ast_parser_func to1 in
  let state1 = C.execute res1 in
  let state1 = S.print_state state1 in state1
     

(*let str0 = " if(c) 
             {a:=1} 
             else 
             {b:=1} "
let res0 = automatedTest str0*)

(*let prog = "a := 100 ; b:= 2; c := 3; d := 4; if (a = 100) { b:=100; c:= 100 } else { while (c = 3) { if (b = 1) { c := 3 } else { b := b - 1 }}}"
let res0 = automatedTest prog

let str0 = " b1 := true; b2 := false; a:=1; b:=200; if(b1) { b:= b + a } else { a:= b + a }"
let res0 = automatedTest str0

let str1 = "a:=1; b:=1; c:=1;while(a){if(c){c:=0;a:=b}else{b:=0;c:=a}}"
let res1 = automatedTest str1*)

let str2 = "a:=1;b:=1;c:=1;d:=1"
let res2 = automatedTest str2

let str3 = "a:=1;b:=1;if(b = a) { b := 2 } else { a := 2}"
let res3 = automatedTest str3

let str4 = "b:=1;a:=1;if(a){c:=0}else{c:=1};if(b){d:=b}else{d:=0}"
let res4 = automatedTest str4

let str5 = "a:=1;b:=5;c:=0;while(a) { c:= c + 1; if (c = 5) { a:= 0 } else { }}"
let res5 = automatedTest str5

let str6 = "a:=1;while(a) { if (a) { a:= 0 } else{a:=0}; d:=3}"
let res6 = automatedTest str6

let str7 = "a:=0 ; b:=5; c:= 0;
            if (a = c) {
              c := a + 100
            } else {
              a := c + 200
            };
            while (b = 5) {
              b := 0
            };
            c := c - 50;
            a := a - b;
            b := 0;
            d := 3333;
            w := true;
            z := false"

let res7 = automatedTest str7

let str5 = "a :=1 ;
                      b :=1 ;
                      c :=1 ;
                      while(a) {
                          if(c) {
                            c := 0 ;
                            a := b
                          } else {
                            b := 0 ;
                            c := a
                          };
            }"
let res5 = automatedTest str5

let str6 = "a := #; if (a) { c := 1 } else { d := 1 }"
let res6 = automatedTest str6
