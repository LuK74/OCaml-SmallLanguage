open AnalyseurLexicale
open BoolExp
open ArithExp

module AL = AnalyseurLexicale
module Ar = ArithExp
module Bo = BoolExp

module AnalyseurSyntaxique =
  struct
    type var = string
    type exp = AExp of Ar.aExp | BExp of Bo.boolExp | Var of var | Hash


    type instr = Assign of var * exp
               | If of exp * instr * instr
               | While of exp * instr
               | Seq of instr * instr
               | Skip

    let print_var = fun v ->
      print_string v

    let print_exp = fun e ->
      match e with
      | Var(k) -> print_var k
      | AExp(ae1) -> Ar.printAExp ae1
      | BExp(be1) -> Bo.printBExp be1
      | Hash -> print_char '#'
 

    let rec print_instr = fun i ->
      match i with
      | Skip -> print_string "(seq_end)\n"
      | Assign(v,o) -> print_string v; print_string " := "; print_exp o; print_string "\n"
      | If(bexp, i1, i2) -> print_string " if("; print_exp bexp; print_string ") \n"
      | While(bexp, i1) -> print_string " while("; print_exp bexp; print_string ") \n"
      | Seq(i1, i2) -> print_instr i1

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

    let p_V : (var, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.Identifier(s), l1) -> (s, l1)
      | _ -> raise (Echec ("Var attendu, a,b,c ou d ", l))


    let p_C : (exp, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.ValueENT(k), l1) -> (AExp(Ar.CoENT(k)), l1)
      | Cons(AL.ValueBOOL(k), l1) -> (BExp(Bo.CoB(k)), l1)
      | Cons(AL.ValueFLOAT(k), l1) -> (AExp(Ar.CoENT(1)), l1) (* not yet implemented *)
      | _ -> raise (Echec ("Const attendu, 0 ou 1 ", l))


    let p_E : (exp, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.Identifier(s), l1) -> let (var,suite) = p_V l in ((Var(var),suite))
      | Cons(AL.ValueENT(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | Cons(AL.ValueBOOL(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | Cons(AL.ValueFLOAT(k), l1) -> let (var, suite) =  p_C l in (var, suite)
      | _ -> raise (Echec ("Exp attendu ", l))

    (* ameliorable *)
    (* recursive à gauche , on commence par les constantes et var pour éviter récursion infini*)
    let rec p_AExp : (exp, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TPlus +> p_Op ++> fun b -> (match b with
                                                 | Var(s) -> return (AExp(Ar.AplENT(Ar.CoENT(0),Ar.VaENT(s))))
                                                 | AExp(a) -> return (AExp(Ar.AplENT(Ar.CoENT(0), a)))
                                                 | _ -> raise (Echec ("Syntax error", l))))
        +|
          (terminal AL.TMinus +> p_Op ++> fun b -> (match b with
                                                 | Var(s) -> return (AExp(Ar.AmiENT(Ar.CoENT(0),Ar.VaENT(s))))
                                                 | AExp(a) -> return (AExp(Ar.AmiENT(Ar.CoENT(0), a)))
                                                 | _ -> raise (Echec ("Syntax error", l))))
        +|
          (terminal AL.TDivide +> p_Op ++> fun b -> (match b with
                                                 | Var(s) -> return (AExp(Ar.AdiENT(Ar.CoENT(0),Ar.VaENT(s))))
                                                 | AExp(a) -> return (AExp(Ar.AdiENT(Ar.CoENT(0), a)))
                                                 | _ -> raise (Echec ("Syntax error", l))))
        +|
          (terminal AL.TMultiply +> p_Op ++> fun b -> (match b with
                                                       | Var(s) -> return (AExp(Ar.AmuENT(Ar.CoENT(0),Ar.VaENT(s))))
                                                       | AExp(a) -> return (AExp(Ar.AmuENT(Ar.CoENT(0), a)))
                                                       | _ -> raise (Echec ("Syntax error", l))))
        +|
          (terminal AL.TModulo +> p_Op ++> fun b -> (match b with
                                                     | Var(s) -> return (AExp(Ar.AmoENT(Ar.CoENT(0),Ar.VaENT(s))))
                                                     | AExp(a) -> return (AExp(Ar.AmoENT(Ar.CoENT(0), a)))
                                                     | _ -> raise (Echec ("Syntax error", l))))
        +|
          (p_V ++> fun a -> return(AExp(Ar.VaENT(a))))
        +|
          (p_C ++> fun a -> (match a with
                             | AExp(a1) -> return(AExp(a1))
                             | _ -> raise (Echec ("INT CONST requested\n", l))))                            
    and p_BExp : (exp, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TEgale +> p_Op ++> fun b ->
                                        match b with
                                        | AExp(a) -> return (BExp(Bo.EqualB(Ar.CoENT(0), a)))
                                        | Var(s) -> return (BExp(Bo.EqualB(Ar.CoENT(0), Ar.VaENT(s))))
                                        | _ -> raise (Echec("Syntax error\n", l)))
        +|
          (p_C ++> fun a -> (match a with
                             | BExp(b) -> return (BExp(b))
                             | _ -> raise (Echec ("BOOL requested\n", l))))
        +|
          (p_V ++> fun a -> return (BExp(Bo.VaB(a))))
    (* +|
          p_AExp ++> fun a -> terminal AL.TEgale +> p_AExp ++> fun b -> return (Bo.EqualB(a,b)))*)

    and p_Op : (exp, AL.token) ranalist = fun l ->
      l |>
        (p_C ++> fun a -> p_AExp ++>
                            fun b ->
                            (match a,b with
                             | (BExp(_), _) -> raise (Echec ("Type error", l))
                             | (AExp(Ar.VaENT(_)), _) -> raise (Echec ("Syntax error", l))
                             | (AExp(Ar.CoENT(_)),_) -> raise (Echec ("Syntax error", l))
                             | (AExp(l),AExp(Ar.AplENT(_, r))) -> return (AExp(Ar.AplENT(l,r)))
                             | (AExp(l),AExp(Ar.AmiENT(_, r))) -> return (AExp(Ar.AmiENT(l,r)))
                             | (AExp(l),AExp(Ar.AdiENT(_, r))) -> return (AExp(Ar.AdiENT(l,r)))
                             | (AExp(l),AExp(Ar.AmuENT(_, r))) -> return (AExp(Ar.AmuENT(l,r)))
                             | (AExp(l),AExp(Ar.AmoENT(_, r))) -> return (AExp(Ar.AmoENT(l,r)))
                             | _ -> raise (Echec ("Syntax error", l))))
        +|
          (p_V ++> fun a -> p_AExp ++>
                              fun b ->
                              (match b with
                               | AExp(Ar.AplENT(_, r)) -> return (AExp(Ar.AplENT(Ar.VaENT(a),r)))
                               | AExp(Ar.AmiENT(_, r)) -> return (AExp(Ar.AmiENT(Ar.VaENT(a),r)))
                               | AExp(Ar.AdiENT(_, r)) -> return (AExp(Ar.AdiENT(Ar.VaENT(a),r)))
                               | AExp(Ar.AmuENT(_, r)) -> return (AExp(Ar.AmuENT(Ar.VaENT(a),r)))
                               | AExp(Ar.AmoENT(_, r)) -> return (AExp(Ar.AmoENT(Ar.VaENT(a),r)))
                               | _ -> raise (Echec ("Syntax error", l))))                            
        +|
          (p_C ++> fun a -> p_BExp ++>
                              fun b ->
                              (match a,b with
                               | (BExp(k),BExp(Bo.OrB(_, b))) -> return (BExp(Bo.OrB(k,b)))
                               | (BExp(k),BExp(Bo.AndB(_, b))) -> return (BExp(Bo.AndB(k,b)))
                               | (AExp(a1),BExp(Bo.EqualB(_, b))) -> return (BExp(Bo.EqualB(a1,b)))
                               | (AExp(_), _) -> raise (Echec ("Type error", l))
                               | _ -> raise (Echec ("Syntax error", l))))
        +|
          (p_V ++> fun a -> p_BExp ++>
                              fun b ->
                              (match b with
                               | BExp(Bo.OrB(_, b)) -> return (BExp(Bo.OrB(Bo.VaB(a),b)))
                               | BExp(Bo.AndB(_, b)) -> return (BExp(Bo.AndB(Bo.VaB(a),b)))
                               | BExp(Bo.EqualB(_, b)) -> return (BExp(Bo.EqualB(Ar.VaENT(a),b)))
                               | _ -> raise (Echec ("Syntax error", l))))
        +|
          (terminal AL.TNegat +> p_Op ++> fun a -> match a with
                                                   | BExp(k) -> return (BExp(Bo.NegB(k)))
                                                   | _ -> raise (Echec ("Type error", l)))
        +|
          (p_V ++> fun a -> return (Var(a)))
        +|
          (p_C ++> fun a -> return (a))
    (* +| terminal AL.THash +> return(Hash)*)


    let rec p_S : (instr, AL.token) ranalist = fun l ->
      l |>
        (p_I ++> fun a -> p_L ++> fun b -> return (Seq(a,b)))
        +| return Skip
    and p_L : (instr, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TPointVir +> p_S ++> fun a -> return(a))
        +| return Skip
    and p_I : (instr, AL.token) ranalist = fun l ->
      l |>
        (terminal AL.TIf +> terminal AL.TParouv +> p_Op ++>
           fun a -> terminal AL.TParfer +> terminal AL.TAccouv +> p_S ++>
                      fun b -> terminal AL.TAccfer +> terminal AL.TElse +> terminal AL.TAccouv +>
                                 p_S ++> fun c -> terminal AL.TAccfer +>
                                                    return (If(a,b,c)))
        +|
          (terminal AL.TWhile +> terminal AL.TParouv +> p_Op ++>
             fun a -> terminal AL.TParfer +> terminal AL.TAccouv +> p_S ++>
                        fun b -> terminal AL.TAccfer +> return (While(a,b)))
        +|
          (p_V ++> fun a -> terminal AL.TAffec +> p_Op ++>
                              fun b -> (return (Assign(a,b))))
        +|
          (return Skip)


    
    let list_of_string s =
      let rec boucle s i n = fun () ->
        if i = n then AL.Nil  else (AL.Cons(s.[i], (boucle s (i+1) n )))
      in boucle s 0 (String.length s)


    let rec convertLazyToRegular = fun (l : 't AL.mylist) -> fun regular ->
                                                             match l() with
                                                             | AL.Nil -> regular
                                                             | AL.Cons(a,l1) -> convertLazyToRegular l1 (regular@[a])


    let ast_parser_func = fun (s1 : 't AL.mylist) ->
      (let parsed_string = p_S s1 in
       match parsed_string with
       | astRes,l -> (match l() with
                      | AL.Nil -> astRes
                      | _ -> raise (Echec ("N'appartient pas à la grammaire", l))))
    
    
    
  end
