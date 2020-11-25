module AnalyseurSyntaxique =
  struct
  type var = char
  type const = int
  type exp = Var of var | Const of const
  type ope = Exp of exp | Hash

  type instr = Assign of var * ope
          | If of exp * instr * instr
          | While of exp * instr
          | Seq of instr * instr
          | Skip

  let print_var = fun v ->
    print_char v

  let print_const = fun c ->
    print_int c

  let print_exp = fun e ->
    match e with
    | Var(k) -> print_var k
    | Const(n) -> print_const n

  let print_ope = fun o ->
    match o with
    | Exp(exp1) -> print_exp exp1
    | Hash -> print_char '#'

  let rec print_instr = fun i ->
    match i with
    | Skip -> print_string "(seq_end)\n"
    | Assign(v,o) -> print_var v; print_string " := "; print_ope o; print_string "\n"
    | If(bexp, i1, i2) -> print_string " if("; print_exp bexp; print_string ") \n"
    | While(bexp, i1) -> print_string " while("; print_exp bexp; print_string ") \n"
    | Seq(i1, i2) -> print_instr i1

  type 'a mylist = unit -> 'a contentsll
  and 'a contentsll = Nil | Cons of 'a * 'a mylist

  exception Echec of string * (char mylist)

  type ('r, 't) ranalist = 't mylist -> 'r * 't mylist
  type ('t) analist = 't mylist -> 't mylist
  type ('x, 't) st = 't mylist -> 'x

  let (+>) (a : ('t) analist) (b : ('x, 't) st) : ('x, 't) st =
  fun l -> let l = a l in b l

  let (++>) (a : ('r, 't) ranalist) (b : 'r -> ('x, 't) st) : ('x, 't) st =
  fun l -> let (ast,l) = a l in b ast l
 
  let (+|) (a : ('x ,'t) st) (b : ('x ,'t) st) : ('x, 't) st =
  fun l -> try a l with (Echec (s1,l1)) -> b l


  let rec consumeBlank : (char) analist = fun l ->
    match l() with
    | Cons(' ', l1) -> consumeBlank l1
    | Cons('\n', l1) -> consumeBlank l1
    | Cons('\t', l1) -> consumeBlank l1
    | _ -> l

  let rec terminal c : (char) analist = fun l ->
    let newList = consumeBlank l in
    match newList() with
    | Cons(x,l1) when x = c -> l1
    | _ -> raise (Echec ("Terminal attendu ", l))

  let return : 'r -> ('r, 't) ranalist =
    fun x l -> let newList = consumeBlank l in
               (x, newList)

  let p_V : (var, char) ranalist = fun l ->
    let newList = consumeBlank l in
    match newList() with
    | Cons('a', l1) -> ('a',l1)
    | Cons('b', l1) -> ('b',l1)
    | Cons('c', l1) -> ('c',l1)
    | Cons('d', l1) -> ('d',l1)
    | _ -> raise (Echec ("Var attendu, a,b,c ou d ", l))


  let p_C : (const, char) ranalist = fun l ->
    let newList = consumeBlank l in
    match newList() with
    | Cons('0', l1) -> (0,l1)
    | Cons('1', l1) -> (1,l1)
    | _ -> raise (Echec ("Const attendu, 0 ou 1 ", l))


  let p_E : (exp, char) ranalist = fun l ->
    let newList = consumeBlank l in
    match newList() with
    | Cons('a', l1) -> let (var,suite) = p_V l in ((Var(var),suite))
    | Cons('b', l1) -> let (var,suite) = p_V l in ((Var(var),suite))
    | Cons('c', l1) -> let (var,suite) = p_V l in ((Var(var),suite))
    | Cons('d', l1) -> let (var,suite) = p_V l in ((Var(var),suite))
    | Cons('0', l1) -> let (const,suite) = p_C l in ((Const(const),suite))
    | Cons('1' ,l1) -> let (const,suite) = p_C l in ((Const(const),suite))
    | _ -> raise (Echec ("Exp attendu ", l))

  let p_O : (ope, char) ranalist = fun l ->
    let newList = consumeBlank l in
    match newList() with
    | Cons('#', l1) -> ((Hash, l1))
    | _ -> let (exp, l1) = p_E l in (Exp(exp),l1)

  (* ameliorable *)
  let rec p_While c : (char) analist = fun l ->
    match l() with
    | Cons(k, suite) when k = c -> if (c = 'w') then (p_While 'h' suite)
                                   else (if (c = 'h') then (p_While 'i' suite)
                                         else (if (c = 'i') then (p_While 'l' suite)
                                               else (if (c = 'l') then (p_While 'e' suite)
                                                     else (if (c = 'e') then suite
                                                           else raise (Echec ("Not a while ", l))))))
    | _ -> raise (Echec ("While attendu ", l))

  let rec p_If c : (char) analist = fun l ->
    match l() with
    | Cons(k, suite) when k = c -> if (c = 'i') then (p_If 'f' suite)
                                   else (if (c = 'f') then suite
                                         else raise (Echec ("Not a if ", l)))
    | _ -> raise (Echec ("If attendu ", l))

   let rec p_Else c : (char) analist = fun l ->
    match l() with
    | Cons(k, suite) when k = c -> if (c = 'e') then (p_Else 'l' suite)
                                   else (if (c = 'l') then (p_Else 's' suite)
                                         else (if (c = 's') then (p_Else 'e' suite)
                                               else (if (c = 'e') then suite
                                                           else raise (Echec ("Not a else ", l)))))
    | _ -> raise (Echec ("Else attendu ", l))

   let rec p_String s : (char) analist = fun l ->
     let newList = consumeBlank l in (
     let rec consumeString s = fun l ->
     match l() with
     | Cons(k,suite) -> if (String.length s > 0)
                        then (if (k = s.[0]) then
                                p_String (String.sub s 1 ((String.length s) - 1)) suite
                              else raise (Echec (("Not string expected"), l)))
                        else l
     | _ -> if (String.length s <= 0) then l else raise (Echec (("Not string expected"), l))
   in consumeString s newList)


  let rec p_S : (instr, char) ranalist = fun l ->
    l |>
      (p_I ++> fun a -> p_L ++> fun b -> return (Seq(a,b)))
      +| return Skip
  and p_L : (instr, char) ranalist = fun l ->
    l |>
      (terminal ';' +> p_S ++> fun a -> return(a))
      +| return Skip
  and p_I : (instr, char) ranalist = fun l ->
    l |>
      (p_String "if" +> terminal '(' +> p_E ++>
         fun a -> terminal ')' +> terminal '{' +> p_S ++>
                    fun b -> terminal '}' +> p_String "else" +> terminal '{' +>
                               p_S ++> fun c -> terminal '}' +>
                                                  return (If(a,b,c)))
      +|
        (p_String "while" +> terminal '(' +> p_E ++>
           fun a -> terminal ')' +> terminal '{' +> p_S ++>
                      fun b -> terminal '}' +> return (While(a,b)))
      +|
        (p_V ++> fun a -> terminal ':' +> terminal '=' +> p_O ++>
                              fun b -> (return (Assign(a,b))))
      +|
        (return Skip)

  
  let list_of_string s =
    let rec boucle s i n = fun () ->
      if i = n then Nil  else (Cons(s.[i], (boucle s (i+1) n )))
    in boucle s 0 (String.length s)

  exception EchecCritique of string * char list

  let rec convertLazyToRegular = fun (l : 't mylist) -> fun regular ->
  match l() with
  | Nil -> regular
  | Cons(a,l1) -> convertLazyToRegular l1 (regular@[a]) 


  let ast_parser_func = fun (s1 : 't mylist) ->
    let parser_try = fun s1 -> 
      (let parsed_string = p_S s1 in
       match parsed_string with
       | astRes,l -> (match l() with
                      | Nil -> astRes
                      | _ -> raise (Echec ("N'appartient pas à la grammaire", l))))
    in
    try parser_try s1 with Echec (s1,l1) -> (raise (EchecCritique("Echec critique, n'appartient pas à la grammaire", (convertLazyToRegular l1 []))))
  
  
  end
