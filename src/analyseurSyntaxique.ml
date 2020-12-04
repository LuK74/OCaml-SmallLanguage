open AnalyseurLexicale
open BoolExp
open ArithExp

module AL = AnalyseurLexicale
module Ar = ArithExp
module Bo = BoolExp

module AnalyseurSyntaxique =
  struct
    (* Utilisé pour l'interprêteur *)
    type var = string
    type exp = AExp of Ar.aExp | BExp of Bo.boolExp | Var of var | Hash | EmptyExp

    (* Utilisé pour représenter une instruction *)
    type instr = Assign of var * exp
               | If of exp * instr * instr
               | Parallele of instr * instr
               | While of exp * instr
               | Seq of instr * instr
               | Skip

    (* Fonction d'affichage utilisé pour le debug *)
    let print_var = fun v ->
      print_string v

    let print_exp = fun e ->
      match e with
      | Var(k) -> print_var k
      | AExp(ae1) -> Ar.printAExp ae1
      | BExp(be1) -> Bo.printBExp be1
      | Hash -> print_char '#'
      | EmptyExp -> print_string " "
 

    let rec print_instr = fun i ->
      match i with
      | Skip -> print_string "(seq_end)\n"
      | Assign(v,o) -> print_string v; print_string " := "; print_exp o; print_string "\n"
      | If(bexp, i1, i2) -> print_string " if("; print_exp bexp; print_string ") \n"
      | While(bexp, i1) -> print_string " while("; print_exp bexp; print_string ") \n"
      | Seq(i1, i2) -> print_instr i1
      | Parallele(i1, i2) -> print_instr i1; print_string " // "; print_instr i2

    exception Echec of string * (AL.token AL.mylist)

    type ('r, 't) ranalist = 't AL.mylist -> 'r * 't AL.mylist
    type ('t) analist = 't AL.mylist -> 't AL.mylist
    type ('x, 't) st = 't AL.mylist -> 'x

    let (+>) (a : ('t) analist) (b : ('x, 't) st) : ('x, 't) st =
      fun l -> let l = a l in b l

    let (++>) (a : ('r, 't) ranalist) (b : 'r -> ('x, 't) st) : ('x, 't) st =
      fun l -> let (ast,l) = a l in b ast l
    
    let (+|) (a : ('x ,'t) st) (b : ('x ,'t) st) : ('x, 't) st =
      fun l -> try a l with (Echec (s1,l1)) -> b l


    let rec terminal c : (AL.token) analist = fun l ->
      match l() with
      | Cons(x,l1) when x = c -> l1
      | _ -> raise (Echec ("Terminal attendu ", l))

    let return : 'r -> ('r, 't) ranalist =
      fun x l -> (x, l)


    (* --------------------------------------------------- Attention ---------------------------------------------------------- *)
    (* ------------- Cette partie doit-être revu, il est possible de l'améliorer et de produire un meilleur code -------------- *)
    
    (* Parse un Identifier *)
    let p_V : (var, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.Identifier(s), l1) -> (s, l1)
      | _ -> raise (Echec ("Var attendu, a,b,c ou d ", l))

    (* Parse une constante *)
    let p_C : (exp, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.ValueENT(k), l1) -> (AExp(Ar.CoENT(k)), l1)
      | Cons(AL.ValueBOOL(k), l1) -> (BExp(Bo.CoB(k)), l1)
      | Cons(AL.ValueFLOAT(k), l1) -> (AExp(Ar.CoENT(1)), l1) (* not yet implemented *)
      | _ -> raise (Echec ("Const attendu, 0 ou 1 ", l))

    (* Parse un Identifier ou une constante *)
    let p_E : (exp, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.Identifier(s), l1) -> let (var,suite) = p_V l in ((Var(var),suite))
      | Cons(AL.ValueENT(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | Cons(AL.ValueBOOL(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | Cons(AL.ValueFLOAT(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | _ -> raise (Echec ("Exp attendu ", l))

    (* Parse les opérateurs d'opération arithmétique *)
    let p_OpeA : (Ar.ope, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.TPlus, l1) -> (Ar.APlus,l1)
      | Cons(AL.TMinus, l1) -> (Ar.AMinus,l1)
      | Cons(AL.TMultiply, l1) -> (Ar.AMultiply,l1)
      | Cons(AL.TDivide, l1) -> (Ar.ADivide,l1)
      | Cons(AL.TModulo, l1) -> (Ar.AModulo,l1)
      | _ -> raise (Echec ("Syntax error\n", l))

    (* Parse les opérateurs binaires d'opération booléenne sur des booléens *)
    let p_OpeBinB : (Bo.opeBinB, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.TOr, l1) -> (Bo.OOr, l1)
      | Cons(AL.TAnd, l1) -> (Bo.OAnd, l1)
      | _ -> raise (Echec ("Syntax error\n", l))

    (* Parse les opérateurs binaires d'opération booléenne sur des expressions arithmétique *)
    let p_OpeBinA : (Bo.opeBinA, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.TEgale, l1) -> (Bo.OEqual, l1)
      | Cons(AL.TGeq, l1) -> (Bo.OGeq, l1)
      | Cons(AL.TGtn, l1) -> (Bo.OGtn, l1)
      | Cons(AL.TLeq, l1) -> (Bo.OLeq, l1)
      | Cons(AL.TLtn, l1) -> (Bo.OLtn, l1)
      | _ -> raise (Echec ("Syntax error\n", l))

        (* Parse les opérateurs unaires d'opération booléenne sur des booléens *)
    let p_OpeUnaB : (Bo.opeUnaB, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.TNegat, l1) -> (Bo.ONeg, l1)
      | _ -> raise (Echec ("Syntax error\n", l))

    (* Inspiré de grammare donné dans l'enoncé de l'extensions 3.2 *)
    let rec p_T : (exp , AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TParouv +> p_Exp ++> fun b -> terminal AL.TParfer +> return (b))                      
        +|
          (terminal AL.TParouv +> p_Exp ++> fun b -> terminal AL.TParfer +> return (b))
        +|
          (p_OpeUnaB ++> fun op -> p_T ++> fun a -> match a with
                                                     | Var(k) -> return (BExp(Bo.UnaOpB(op, Bo.VaB(k))))
                                                     | BExp(k) -> return (BExp(Bo.UnaOpB(op, k)))
                                                     | _ -> raise (Echec ("Type error", l)))
        +|
          (p_V ++> fun a -> return (Var(a)))
        +|
          (p_C ++> fun a -> return (a))
        +|
          (terminal AL.THash +> return(Hash))
    and p_Exp : (exp, AL.token) ranalist = fun l ->
      l |>
        (p_T ++> fun a ->
                 p_AExp ++> fun b ->
                            (match a, b with
                             | (BExp(_), _) -> raise (Echec ("Type error", l))
                             | (Var(s), AExp(Ar.BinOpENT(op, _, a2))) -> return (AExp( Ar.BinOpENT(op, Ar.VaENT(s) ,a2)))
                             | (AExp(l),AExp(Ar.BinOpENT(op, _, r))) -> return (AExp(Ar.BinOpENT(op,l,r)))
                             | (AExp(l),EmptyExp) -> return (AExp(l))
                             | _ -> raise (Echec ("Syntax error", l))))
    
        +|
          (p_T ++> fun a ->
                   p_BExp ++> fun b ->
                              (match a,b with
                               | (Var(s),BExp(Bo.BinOpA(op, _, r))) -> return (BExp(Bo.BinOpA(op,  Ar.VaENT(s), r)))
                               | (Var(s),BExp(Bo.BinOpB(op, _, r))) -> return (BExp(Bo.BinOpB(op, Bo.VaB(s), r)))
                               | (BExp(l),BExp(Bo.BinOpB(op, _, r))) -> return (BExp(Bo.BinOpB(op,l,r)))
                               | (AExp(l),BExp(Bo.BinOpA(op, _, r))) -> return (BExp(Bo.BinOpA(op,l,r)))
                               | _ -> raise (Echec ("Syntax error", l))))                                                   
    and p_AExp : (exp, AL.token) ranalist = fun l ->
      l |>
        (p_OpeA ++> fun op ->
                    p_T ++> fun b ->
                            p_AExp ++> fun c ->
                                       (match b,c with
                                        | (Var(s), EmptyExp) -> return (AExp(Ar.BinOpENT(op, Ar.CoENT(0), Ar.VaENT(s))))
                                        | (Var(s), AExp(Ar.BinOpENT(op2, _, a2))) -> return (AExp(Ar.BinOpENT(op, Ar.CoENT(0) , Ar.BinOpENT(op, Ar.VaENT(s) ,a2))))
                                        | (AExp(a), EmptyExp) -> return (AExp(Ar.BinOpENT(op, Ar.CoENT(0), a)))
                                        | (AExp(a1), AExp(Ar.BinOpENT(op2, _, a2))) -> return (AExp(Ar.BinOpENT(op, Ar.CoENT(0) , Ar.BinOpENT(op, a1 ,a2))))
                                        | _ -> raise (Echec ("Syntax error", l))))
        +|
          (return (EmptyExp))
    and p_BExp : (exp, AL.token) ranalist = fun l ->
      l |>
        (p_OpeBinA ++> fun op ->
                       p_T ++> fun b ->
                               p_AExp ++> fun c ->
                                          (match b,c with
                                           | (Var(s), EmptyExp) -> return (BExp(Bo.BinOpA(op, Ar.CoENT(0), Ar.VaENT(s))))
                                           | (Var(s) , AExp(Ar.BinOpENT(op2, _, r))) -> return (BExp(Bo.BinOpA(op, Ar.CoENT(0), Ar.VaENT(s))))
                                           | (AExp(a), EmptyExp) -> return (BExp(Bo.BinOpA(op, Ar.CoENT(0), a)))
                                           | (AExp(l),AExp(Ar.BinOpENT(op2, _, r))) -> return (BExp(Bo.BinOpA(op, Ar.CoENT(0), Ar.BinOpENT(op2,l,r))))
                                           | _ -> raise (Echec("Syntax error\n", l))))
        +|
          (p_OpeBinB ++> fun op ->
                         p_T ++> fun b ->
                                 p_BExp ++> fun c ->
                                            (match b,c with
                                             | (Var(s), EmptyExp) -> return (BExp(Bo.BinOpB(op, Bo.CoB(true), Bo.VaB(s))))
                                             | (Var(s) , BExp(Bo.BinOpB(op2, _, r))) -> return (BExp(Bo.BinOpB(op, Bo.CoB(true), Bo.VaB(s))))
                                             | (BExp(a), EmptyExp) -> return (BExp(Bo.BinOpB(op, Bo.CoB(true), a)))
                                             | (BExp(l),BExp(Bo.BinOpB(op2, _, r))) -> return (BExp(Bo.BinOpB(op, Bo.CoB(true), Bo.BinOpB(op2,l,r))))
                                             | _ -> raise (Echec("Syntax error\n", l))))
        +|
          (return (EmptyExp))


    (* --------------------------------------------------------------- ---------------------------------------------------------- *)

    

    (* Grammaire principale de notre langage *)
    let rec p_S : (instr, AL.token) ranalist = fun l ->
      l |>
        (p_I ++> fun a -> p_P ++> fun b -> return (Parallele(a,b)))
        +|
          (p_I ++> fun a -> p_L ++> fun b -> return (Seq(a,b)))
        +| return Skip
    and p_L : (instr, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TPointVir +> p_S ++> fun a -> return(a))
        +|
          (return Skip)    
    and p_P : (instr, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TParallele +> p_S ++> fun a -> return (a))
    (*+|
          (return Skip)*) (* Etant donné que p_P posséde la régle "epsilon", et qu'il est 
                                positionné après p_L dans les régles de p_S
                                Il génerera le Skip si p_S représente juste une instruction *)  
    and p_I : (instr, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TIf +> terminal AL.TParouv +> p_T ++>
           fun a -> terminal AL.TParfer +> terminal AL.TAccouv +> p_S ++>
                      fun b -> terminal AL.TAccfer +> terminal AL.TElse +> terminal AL.TAccouv +>
                                 p_S ++> fun c -> terminal AL.TAccfer +>
                                                    return (If(a,b,c)))
        +|
          (terminal AL.TWhile +> terminal AL.TParouv +> p_T ++>
             fun a -> terminal AL.TParfer +> terminal AL.TAccouv +> p_S ++>
                        fun b -> terminal AL.TAccfer +> return (While(a,b)))
        +|
          (p_V ++> fun a -> terminal AL.TAffec +> p_T ++>
                              fun b -> (return (Assign(a,b))))
        +|
          (return Skip)


   
    (* Genére une mylist à partir d'un string *)
    let list_of_string s =
      let rec boucle s i n = fun () ->
        if i = n then AL.Nil  else (AL.Cons(s.[i], (boucle s (i+1) n )))
      in boucle s 0 (String.length s)

    (* Parse une mylist de token pour genére l'AST *)
    let ast_parser_func = fun (s1 : 't AL.mylist) ->
      (let parsed_string = p_S s1 in
       match parsed_string with
       | astRes,l -> (match l() with
                      | AL.Nil -> astRes
                      | _ -> raise (Echec ("N'appartient pas à la grammaire", l))))
    
    
    
  end
