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

  let rec generate_substring = fun list ->
    fun s ->
    match list() with
    | Cons(' ', l1) -> (s, list)
    | Cons('\n', l1) -> (s, list)
    | Cons('\t', l1) -> (s, list)
    | Cons(';', l1) -> if (String.length s > 0) then (s, list) else (";", l1)
    | Cons(':', l1) when (String.length s > 0) -> (s, list)
    | Cons(c, suite) -> generate_substring suite (String.concat "" ([s]@[(String.make 1 c)]))
    | Nil -> (s, list)

  exception NotEntValue of string

  let rec create_valueENT = fun tokenStr ->
    fun value ->
    if (String.length tokenStr > 0) then
      (let c = String.get tokenStr 0 in
      let charCode = Char.code c in
      if (charCode > (Char.code '9')) then raise (NotEntValue tokenStr)
      else (if (charCode < (Char.code '0')) then raise (NotEntValue tokenStr)
            else (create_valueENT (String.sub tokenStr 1 ((String.length tokenStr) - 1 )) value * 10 + (charCode - (Char.code '0')))))
    else value

  let rec create_valueFLOAT = fun tokenStr ->
    fun value -> 0.0

  exception WrongIdentifierSyntax
  let rec checkIdentifier = fun tokenStr ->
    if (String.length tokenStr > 0) then
      (let c = String.get tokenStr 0 in
       let charCode = Char.code c in
       if (charCode > (Char.code 'z')) then
         (if (charCode > (Char.code 'Z')) then raise WrongIdentifierSyntax
          else (if (charCode < (Char.code 'A')) then raise WrongIdentifierSyntax
                else (checkIdentifier (String.sub tokenStr 1 ((String.length tokenStr) - 1 )))))
       else (if (charCode < Char.code 'a') then raise WrongIdentifierSyntax
                else (String.sub tokenStr 1 ((String.length tokenStr) - 1 ))))
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
    | "+" -> TPlus
    | "-" -> TMinus
    | "/" -> TDivide
    | "%" -> TModulo
    | "#" -> THash
    | "true" -> ValueBOOL(true)
    | "false" -> ValueBOOL(false)
    | "True" -> ValueBOOL(true)
    | "False" -> ValueBOOL(false)
    | _ -> let c = String.get tokenStr 0 in
           if (Char.code c <= Char.code '9') then (if (Char.code c >= Char.code '0') then
                                                     (if (String.contains tokenStr '.') then
                                                        ValueFLOAT(create_valueFLOAT tokenStr 0.0)
                                                      else
                                                        ValueENT(create_valueENT tokenStr 0))
                                                   else
                                                     (let testStr = checkIdentifier tokenStr in
                                                      if (String.length testStr = 0) then
                                                        Identifier(tokenStr)
                                                      else
                                                        raise WrongIdentifierSyntax))
  
           else (let testStr = checkIdentifier tokenStr in
                 if (String.length testStr = 0) then
                   Identifier(tokenStr)
                 else
                   raise WrongIdentifierSyntax)


  let rec token_generator = fun list ->
    let newList = consumeBlank list in
    let (substr, next) = generate_substring newList "" in  
    (fun () -> Cons(create_token substr, token_generator next)) 


   let list_of_string s =
    let rec boucle s i n = fun () ->
      if i = n then Nil  else (Cons(s.[i], (boucle s (i+1) n )))
    in boucle s 0 (String.length s)
  
end

module AL = AnalyseurLexicale

let prog = "a := 1; b:= 2; c := 3; d := 4"
let l1 = AL.list_of_string prog
let t1 = AL.token_generator l1
let _ = print_string "Token list : \n"
let r1 = AL.print_token_list t1
let _ = print_string "\n"
