open AnalyseurLexicale
open BoolExp
open ArithExp

module AL = AnalyseurLexicale
module Ar = ArithExp
module Bo = BoolExp

module AnalyseurSyntaxique =
  struct
    type var = string
    type const = ConstENT of int | ConstFLOAT of float | ConstBOOL of bool
    type exp = Var of var | Const of const
    type ope = AExp of Ar.aExp | BExp of Bo.boolExp | Hash

    type instr = Assign of var * ope
               | If of ope * instr * instr
               | While of ope * instr
               | Seq of instr * instr
               | Skip

    let print_var = fun v ->
      match v with
      | Var(s) -> print_string s
      | _ -> print_string " "
    
    let print_const = fun c ->
      match c with
      | ConstENT(k) -> print_int k
      | ConstFLOAT(k) -> print_float k
      | ConstBOOL(k) -> if (k = true) then print_string "true" else print_string "false"


    let print_exp = fun e ->
      match e with
      | Var(k) -> print_var (Var(k))
      | Const(k) -> print_const k

    let print_ope = fun o ->
      match o with
      | AExp(ae1) -> Ar.printAExp ae1
      | BExp(be1) -> Bo.printBExp be1
      | Hash -> print_char '#'

    let rec print_instr = fun i ->
      match i with
      | Skip -> print_string "(seq_end)\n"
      | Assign(v,o) -> print_string v; print_string " := "; print_ope o; print_string "\n"
      | If(bexp, i1, i2) -> print_string " if("; print_ope bexp; print_string ") \n"
      | While(bexp, i1) -> print_string " while("; print_ope bexp; print_string ") \n"
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


    let p_C : (const, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.ValueENT(k), l1) -> (ConstENT(k), l1)
      | Cons(AL.ValueBOOL(k), l1) -> (ConstBOOL(k), l1)
      | Cons(AL.ValueFLOAT(k), l1) -> (ConstFLOAT(k), l1)
      | _ -> raise (Echec ("Const attendu, 0 ou 1 ", l))


    let p_E : (exp, AL.token) ranalist = fun l ->
      match l() with
      | Cons(AL.Identifier(s), l1) -> let (var,suite) = p_V l in ((Var(var),suite))
      | Cons(AL.ValueENT(k), l1) -> let (var, suite) =  p_C l in (Const(var), suite)
      | Cons(AL.ValueBOOL(k), l1) -> let (var, suite) =  p_C l in (Const(var), suite)
      | Cons(AL.ValueFLOAT(k), l1) -> let (var, suite) =  p_C l in (Const(var), suite)
      | _ -> raise (Echec ("Exp attendu ", l))

    (* ameliorable *)
    (* recursive à gauche , on commence par les constantes et var pour éviter récursion infini*)
    let rec p_AExp : (Ar.aExp, AL.token) ranalist = fun l ->
      l |>
        (p_C ++> fun a -> (match a with
                          | ConstENT(k) -> return (Ar.CoENT(k))
                          | _ -> raise (Echec ("INT requested\n", l))))
        +|
          (p_V ++> fun a -> return (Ar.VaENT(a)))
        +|
          (p_AExp ++> fun a -> terminal AL.TPlus +> p_AExp ++> fun b -> return (Ar.AplENT(a,b)))
        +|
          (p_AExp ++> fun a -> terminal AL.TMinus +> p_AExp ++> fun b -> return (Ar.AmiENT(a,b)))
        +|
          (p_AExp ++> fun a -> terminal AL.TDivide +> p_AExp ++> fun b -> return (Ar.AdiENT(a,b)))
        +|
          (p_AExp ++> fun a -> terminal AL.TMultiply +> p_AExp ++> fun b -> return (Ar.AmuENT(a,b)))
        +|
          (p_AExp ++> fun a -> terminal AL.TModulo +> p_AExp ++> fun b -> return (Ar.AmoENT(a,b)))

    let rec p_BExp : (Bo.boolExp, AL.token) ranalist = fun l ->
      l |>
        (p_C ++> fun a -> (match a with
                           | ConstBOOL(k) -> return (Bo.CoB(k))
                           | _ -> raise (Echec ("BOOL requested\n", l))))
        +|
          (p_V ++> fun a -> return (Bo.VaB(a)))
        +|
          (p_AExp ++> fun a -> terminal AL.TEgale +> p_AExp ++> fun b -> return (Bo.EqualB(a,b)))


    let p_Op : (ope, AL.token) ranalist = fun l ->
      l |>
        (p_AExp ++> fun a -> return (AExp(a)))
        +| (p_BExp ++> fun b -> return (BExp(b)))
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
