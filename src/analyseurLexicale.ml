module AnalyseurLexicale =
struct
  type token =
    | TSkip
    | TWhile
    | TIf
    | TElse
    | TPointVir
    | TParouv
    | TParfer
    | TAccouv
    | TAccfer
    | TAffec
    | TEgale
    | TNegat
    | TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TModulo
    | THash
    | ValueENT of int
    | ValueFLOAT of float
    | ValueBOOL of bool
    | Identifier of string

  type 'a mylist = unit -> 'a contentsll
  and 'a contentsll = Nil | Cons of 'a * 'a mylist

  let print_token = fun t ->
    match t with
    | TSkip -> print_string ""; t
    | TWhile -> print_string "while"; t
    | TIf -> print_string "if"; t
    | TElse -> print_string "else"; t
    | TPointVir -> print_string ";"; t
    | TParouv -> print_string "("; t
    | TParfer -> print_string ")"; t
    | TAccouv -> print_string "{"; t
    | TAccfer -> print_string "}"; t
    | TAffec -> print_string ":="; t
    | TEgale -> print_string "="; t
    | TNegat -> print_string "!"; t
    | TPlus -> print_string "+"; t
    | TMinus -> print_string "-"; t
    | TMultiply -> print_string "*"; t
    | TDivide -> print_string "/"; t
    | TModulo -> print_string "%"; t
    | THash -> print_string "#"; t
    | ValueENT(k) -> print_int k; t
    | ValueFLOAT(k) -> print_float k; t
    | ValueBOOL(k) -> print_string (if (k = true) then "true" else "false"); t
    | Identifier(s) -> print_string s; t

  let rec print_token_list = fun list ->
    match list() with
    | Cons(t, suite) -> let res = print_token t in print_token_list suite
    | _ -> print_string "\n Prog End \n"

  let rec consumeBlank = fun l ->
    match l() with
    | Cons(' ', l1) -> consumeBlank l1
    | Cons('\n', l1) -> consumeBlank l1
    | Cons('\t', l1) -> consumeBlank l1
    | _ -> l


  let isNumb = fun c ->
    let charCode = Char.code c in
    if (charCode > Char.code '9') then false
    else ((if (charCode < Char.code '0') then false
          else true))

  let rec generate_subvalue = fun list ->
    fun s ->
    match list() with
    | Cons('.', suite) -> if (String.contains s '.') then (s, list)
                          else generate_subvalue suite (String.concat "" ([s]@[(String.make 1 '.')]))
    | Cons(c, suite) -> if (isNumb c) then generate_subvalue suite (String.concat "" ([s]@[(String.make 1 c)]))
                        else (s, list)
    | _ -> (s, list)

  let isChar = fun c ->
    let charCode = Char.code c in
    if (charCode > Char.code 'z') then false
    else (if (charCode >= Char.code 'a') then true
          else (if (charCode > Char.code 'Z') then false
                else (if (charCode >= Char.code 'A') then true
                      else false)))
  
  let rec generate_subword = fun list ->
    fun s ->
    match list() with
    | Cons(c, suite) ->  if (isChar c) then generate_subword suite (String.concat "" ([s]@[(String.make 1 c)]))
                         else (s,list)
    | Nil -> (s, list)
    

  let rec generate_substring = fun list ->
    fun s ->
    match list() with
    | Cons(' ', l1) -> (s, list)
    | Cons('\n', l1) -> (s, list)
    | Cons('\t', l1) -> (s, list)
    | Cons('(' , l1) -> if (String.length s > 0) then (s, list) else ("(", l1)
    | Cons(')', l1) -> if (String.length s > 0) then (s, list) else (")", l1)
    | Cons('{', l1) -> if (String.length s > 0) then (s, list) else ("{", l1)
    | Cons('}', l1) -> if (String.length s > 0) then (s, list) else ("}", l1)
    | Cons('!', l1) -> if (String.length s > 0) then (s, list) else ("!", l1)
    | Cons('=', l1) -> if (String.length s > 0) then (if (String.get s 0 = ':') then (":=", l1)
                                                     else (s, list))
                       else ("=", l1)
    | Cons(';', l1) -> if (String.length s > 0) then (s, list) else (";", l1)
    | Cons(c, suite) -> generate_substring suite (String.concat "" ([s]@[(String.make 1 c)]))
    | Nil -> (s, list)

  let generate_subtoken = fun list ->
    match list() with
    | Cons(c, suite) -> if (isNumb c) then (generate_subvalue list "")
                        else (if (isChar c) then (generate_subword list "")
                              else (generate_substring list ""))
    | Nil -> ("", list)
  
  exception NotEntValue of string

  let rec create_valueENT = fun tokenStr ->
    fun value ->
    if (String.length tokenStr > 0) then
      (let c = String.get tokenStr 0 in
       if (isNumb c) then (create_valueENT (String.sub tokenStr 1 ((String.length tokenStr) - 1 )) (value * 10 + (Char.code c - (Char.code '0'))))
       else raise (NotEntValue tokenStr))
    else value

  let rec create_valueFLOAT = fun tokenStr ->
    fun value -> 0.0

  exception WrongIdentifierSyntax of string
  let rec checkIdentifier = fun tokenStr ->
    if (String.length tokenStr > 0) then
      (let c = String.get tokenStr 0 in
       if (isChar c) then  (String.sub tokenStr 1 ((String.length tokenStr) - 1 ))
       else raise (WrongIdentifierSyntax tokenStr))
    else tokenStr

  let create_token = fun tokenStr ->
    match tokenStr with
    | "" -> TSkip
    | "if" -> TIf
    | "If" -> TIf
    | "else" -> TElse
    | "Else" -> TElse
    | "While" -> TWhile
    | "while" -> TWhile
    | ";" -> TPointVir
    | ":=" -> TAffec
    | "(" -> TParouv
    | ")" -> TParfer
    | "{" -> TAccouv
    | "}" -> TAccfer
    | "=" -> TEgale
    | "!" -> TNegat
    | "+" -> TPlus
    | "*" -> TMultiply
    | "-" -> TMinus
    | "/" -> TDivide
    | "%" -> TModulo
    | "#" -> THash
    | "true" -> ValueBOOL(true)
    | "false" -> ValueBOOL(false)
    | "True" -> ValueBOOL(true)
    | "False" -> ValueBOOL(false)
    | _ -> let c = String.get tokenStr 0 in
           if (isNumb c) then (if (String.contains tokenStr '.') then
                                 ValueFLOAT(create_valueFLOAT tokenStr 0.0)
                               else
                                 ValueENT(create_valueENT tokenStr 0))
           else (let testStr = checkIdentifier tokenStr in
                 if (String.length testStr = 0) then
                   Identifier(tokenStr)
                 else
                   raise (WrongIdentifierSyntax tokenStr))
  


  let rec token_generator = fun list ->
    let newList = consumeBlank list in
    let (substr, next) = generate_subtoken newList in
    if (next() = Nil) then (fun () -> Cons(create_token substr,fun () -> Nil))
    else (fun () -> Cons(create_token substr, token_generator next)) 

  

   let list_of_string s =
    let rec boucle s i n = fun () ->
      if i = n then Nil  else (Cons(s.[i], (boucle s (i+1) n )))
    in boucle s 0 (String.length s)
  
end
(*
module AL = AnalyseurLexicale

let prog = "a:=1;b:=1;c:=1;d:=1;a:=b + d"
let l1 = AL.list_of_string prog
let t1 = AL.token_generator l1
let _ = print_string "Token list : \n"
let r1 = AL.print_token_list t1
let _ = print_string "\n"
 *)
