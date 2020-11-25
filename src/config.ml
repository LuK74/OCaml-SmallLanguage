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
      fun flag ->
      if (flag = 1) then
        (print_string "(debug) ";
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
  print_string "Execution starts : \n";
  print_string "Prog : \n";
  print_string s;
  print_string "\n";
  let ch1 = A.list_of_string s in
  let res1 = A.ast_parser_func ch1 in
  let state1 = C.execute res1 in
  let state1 = S.print_state state1 in state1
     

let str0 = " if(c) 
             {a:=1} 
             else 
             {b:=1} "
let res0 = automatedTest str0

let str1 = "a:=1; b:=1; c:=1;while(a){if(c){c:=0;a:=b}else{b:=0;c:=a}}"
let res1 = automatedTest str1

let str2 = "a:=1;b:=1;c:=1;d:=1"
let res2 = automatedTest str2

let str3 = "while(a){a:=0;while(b){b:=0;while(c){c:=0;while(d){d:=0}}}}"
let res3 = automatedTest str3

let str4 = "if(a){c:=0}else{c:=1};if(b){d:=1}else{d:=0}"
let res4 = automatedTest str4

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
                          }
            }\n"
let res5 = automatedTest str5

let str6 = "a := #; if (a) { c := 1 } else { d := 1 }"
let res6 = automatedTest str6
