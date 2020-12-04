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

module Natural =
  struct

    exception InstrNotAssign
    (* Exec_affec a été passé dans Config car le laisser dans State créer des
       dépendances mutuelles entre State ArithExp et BoolExp *)
    let exec_affec = fun instr ->
      fun s ->
      match instr with
      | A.Assign(c,ope) -> (match ope with
                            | A.Hash -> (let res = (S.read_var c s) in
                                         (match res with
                                          | S.VEnt(k) -> (S.modify_var c (S.VEnt(-k)) s)
                                          | S.VFloat(k) -> (S.modify_var c (S.VFloat(-.k)) s)
                                          | S.VBool(k) -> (match k with
                                                           | true -> (S.modify_var c (S.VBool(false)) s)
                                                           | false -> (S.modify_var c (S.VBool(true)) s))))
                            | A.AExp(ae1) -> (let res = (Ar.evalAExp ae1 s) in
                                              let newVal = (S.VEnt(res)) in
                                              (S.modify_var c newVal s))
                            | A.BExp(be1) -> (let res = (Bo.evalBExp be1 s) in
                                              let newVal = (S.VBool(res)) in
                                              S.modify_var c newVal s)
                            | A.Var(v) -> S.modify_var c (S.read_var v s) s
                            | _ -> raise InstrNotAssign)
      | _ -> raise InstrNotAssign

     exception StepError of S.state

     (* Simplifie l'evaluation d'une expression en tant que condition 
        Si l'evaluation de l'expression donne un entier :
          - Si l'entier est nulle, on renvoie faux
          - Si l'entier est non nulle, on renvoie true
        Si l'evalutation de l'expression donne un booléen on renvoie
        juste le booléen *)
     let evalResBExp = fun exp ->
       fun s1 ->
      match exp with
      | A.BExp(bexp) -> Bo.evalBExp bexp s1
      | A.AExp(aexp) -> (let resultA = Ar.evalAExp aexp s1 in
                         (if (resultA = 0) then false
                          else true))
      | A.Var(v) -> (let resEval = S.read_var v s1 in
                     match resEval with
                     | VBool(k) -> k
                     | VEnt(k) -> if (k = 0) then false else true
                     | _ -> raise (StepError s1))
      | A.Hash -> raise (StepError s1)
      | _ -> raise (StepError s1)

     exception ExecError of S.state
     (* Execute le programme dans son intégralité *)
     let rec execute_aux = fun i1 ->
      fun s1 ->
      try (
        match i1 with
        | A.Skip -> s1
        | A.Assign(id, ope) -> exec_affec i1 s1
        | A.Seq(i1,i2) -> (let res = execute_aux i1 s1 in
                           execute_aux i2 res)
        | A.While(expB, i1) when (evalResBExp expB s1 = true) ->
           (let res = execute_aux i1 s1 in
            execute_aux (A.While (expB, i1)) res)
        | A.While(expB, i1) when (evalResBExp expB s1 = false) ->
           s1
        | A.If(expB, i1, i2) when (evalResBExp expB s1 = true) ->
           execute_aux i1 s1
        | A.If(expB, i1, i2) when (evalResBExp expB s1 = false) ->
           execute_aux i2 s1
        | A.Parallele(i1, i2) -> let random = Random.int 2 in
                                 (if (random == 0) then (execute_aux (A.Seq(i1,i2)) s1)
                                  else (execute_aux (A.Seq (i1, i2)) s1))
        | _ -> (raise (ExecError s1)))
      with S.VarNotFound(ch, st) -> print_string "Looking for "; print_string ch; print_string " on state : "; S.print_state st; (raise (ExecError st))

     (* Lances execute_aux *)
    let execute = fun i1 ->
      print_string "Execution is starting\n";
      let res = execute_aux i1 (S.create_state) in
      print_string "[FINAL STATE]  "; S.print_state res; res
   
  end

module N = Natural


let _ =  print_string "Would you like to use your personnal seed for random generation ?\n";
         print_string "(Seed) [If you enter 0, we'll use our own seed, or -1 for a random one] : ";
         let seed = read_int() in
         print_string "\nSeed used : ";
         (if (seed = 0) then ((Random.init(7121999));print_string "7121999 \n\n")
          else (if (seed = -1) then (Random.self_init(); print_string "Random seed \n\n")
                else (Random.init seed; print_int seed; print_string "\n\n")))

(* Fonction pour automatisé des test de programme *)
let automatedTest = fun s ->
  print_string "\n-------------------------------------\n";
  print_string "\nExecution starts \n";
  print_string "Prog : \n>";
  print_string s;
  print_string "\n";
  print_string "\n";
  let to1 = Al.analex s in
  print_string "Tokens : \n"; Al.print_token_list to1; print_string "\n";
  let res1 = A.ast_parser_func to1 in
  let state1 = N.execute res1 in
  state1
     

let str2 = "a:=1;b:=1;c:=1;d:=1"
let res2 = automatedTest str2

let str3 = "a:=1;b:=1;if((b = a)) { b := 2 } else { a := 2}"
let res3 = automatedTest str3

let str4 = "b:=1;a:=1;if(a){c:=0}else{c:=1};if(b){d:=b}else{d:=0}"
let res4 = automatedTest str4

let str5 = "a:=1;b:=5;c:=0;while(a) { c:= (c + 1); if ((c = 5)) { a:= 0 } else { }}"
let res5 = automatedTest str5

let str6 = "a:=1;while(a) { if (a) { a:= 0 } else{a:=0}; d:=3}"
let res6 = automatedTest str6

let str7 = "a:=0 ; b:=5; c:= 0;
            if ((a = c)) {
              c := ((a + 100))
            } else {
              a := ((c + 200))
            };
            while ((b = 5)) {
              b := 0
            };
            c := ((c - 50));
            a := ((a - b));
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

let _ = print_string "\n";
