module AnalyseurLexicale =
  struct

    (* Correspond à une lazylist *)
    type 'a mylist = unit -> 'a contentsll
    and 'a contentsll = Nil | Cons of 'a * 'a mylist

    (* Renvoie la valeur d'un entier représenté par un char *)
    let valueChar = fun c ->
      let valueC = Char.code c in
      (valueC - Char.code '0')    

    (* Renvoie true si c est un chiffre
       sinon renvoie false *)
    let isNumb = fun c ->
      let charCode = Char.code c in
      if (charCode > Char.code '9') then false
      else ((if (charCode < Char.code '0') then false
             else true))

    (* Renvoie true si c est un char alphabétique
       sinon renvoie false *)
    let isChar = fun c ->
      let charCode = Char.code c in
      if (charCode > Char.code 'z') then false
      else (if (charCode >= Char.code 'a') then true
            else (if (charCode > Char.code 'Z') then false
                  else (if (charCode >= Char.code 'A') then true
                        else false)))

    (* Génere un string à partir
       d'une mylist *)
    let rec string_of_list l =
      match l() with
      | Nil -> ""
      | Cons(c,l) -> String.make 1 c ^ string_of_list l

    (* Génere une mylist à partir
       d'un string *)
    let list_of_string s =
      let rec boucle s i n = fun () ->
        if i = n then Nil  else (Cons(s.[i], (boucle s (i+1) n )))
      in boucle s 0 (String.length s)

    exception LexicalError of string

    (* Créer un nombre entier à partir d'une chaine de caractère *)
    let rec create_valueENT = fun tokenStr ->
      fun value ->
      if (String.length tokenStr > 0) then
        (let c = String.get tokenStr 0 in
         if (isNumb c) then (create_valueENT (String.sub tokenStr 1 ((String.length tokenStr) - 1 )) (value * 10 + (Char.code c - (Char.code '0'))))
         else raise (LexicalError tokenStr))
      else value
    
    let rec create_valueFLOAT = fun tokenStr ->
      raise (LexicalError "Not yet implemented\n")



  exception LexicalParsingEchec of string
  type 't analist = 't mylist -> 't mylist
  type ('r, 't) ranalist = 't mylist -> 'r * 't mylist

  (* Parse un terminal de la grammaire 
     On vérifie que le char est bien
     égale à c
   *)
  let terminal c : 't analist = fun l ->
    match l() with
    | Cons(x, l) when x = c -> l
    | _ -> raise (LexicalParsingEchec "Wrong terminal\n")

  (* Parse un terminal de la grammaire 
     sans vérifier qu'il correspond bien 
     à un char attendu *)
  let anyterminal : ('t, 't) ranalist = fun l ->
    match l() with
    | Cons(x, l)  -> (x, l)
    | _ -> raise (LexicalParsingEchec "Empty list\n")

  (* Analyseur correspondant à un non-terminal vide *)
  let epsilon : 't analist = fun l -> l

  (* Un epsilon informatif *)
  let return (info : 'r) : ('r, 't) ranalist =
    fun l -> (info, l)

  (* a suivi de b, ce dernier pouvant rendre un résultat *)
  let (+>) (a : 't analist) (b : 't mylist -> 'x) : 't mylist -> 'x =
    fun l -> let l = a l in b l

  (* a rendant un résultat suivi de b, ce dernier pouvant rendre un résultat *)
  let (++>) (a : ('r, 't) ranalist) (b : 'r -> 't mylist -> 'x) : 't mylist -> 'x =
    fun l -> let (x, l) = a l in b x l

  (* Choix entre a ou b *)
  let (+|) (a : 't mylist -> 'x) (b : 't mylist -> 'x) : 't mylist -> 'x =
    fun l -> try a l with LexicalParsingEchec s -> b l

  (* Version fourni de isNumb et isChar *)
  let lettre c = 
    match c with
    | 'a'..'z' | 'A'..'Z' -> c
    | _ -> raise (LexicalParsingEchec "Not a letter\n")

  let chiffre c =
    match c with
    | '0'..'9' -> c
    | _ -> raise  (LexicalParsingEchec "Not a number\n")

  (* Parse une lettre *)
  let p_lettre : (char, char) ranalist =
    anyterminal ++> fun c -> return (lettre c)

  (* Parse un chiffre *)
  let p_chiffre : (char, char) ranalist =
    anyterminal ++> fun c -> return (chiffre c)

  (* Parse un chiffre ou lettre *)
  let p_alphanum : (char, char) ranalist =
    p_lettre +| p_chiffre

  (* Parse une chaine de nombre et/ou char *)
  let rec p_alphanums : (char mylist, char) ranalist = fun l ->
    l |>
      (p_alphanum ++> fun a -> p_alphanums ++> fun la -> return (fun () -> (Cons(a, la))))
      +| (return (fun () -> Nil))

  (* Parse un nombre *)
  let rec p_chiffres : (char mylist, char) ranalist = fun l ->
    l |>
      (p_chiffre ++> fun c -> p_chiffres ++> fun lc -> return (fun () -> (Cons(c, lc))))
      +| (return (fun() -> Nil))

  (* Liste des token *)
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
    | TOr
    | TAnd
    | TGtn
    | TLtn
    | TGeq
    | TLeq
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

  (* Affiche le string correspondant à un token *)
  let print_token = fun t ->
    match t with
    | TSkip -> print_string ""
    | TWhile -> print_string "while"
    | TIf -> print_string "if" 
    | TElse -> print_string "else"
    | TPointVir -> print_string ";" 
    | TParouv -> print_string "(" 
    | TParfer -> print_string ")" 
    | TAccouv -> print_string "{" 
    | TAccfer -> print_string "}" 
    | TAffec -> print_string ":=" 
    | TEgale -> print_string "=" 
    | TNegat -> print_string "!" 
    | TOr -> print_string "||" 
    | TAnd -> print_string "&&"
    | TGtn -> print_string ">"
    | TLtn -> print_string "<"
    | TGeq -> print_string ">="
    | TLeq -> print_string "<="
    | TPlus -> print_string "+" 
    | TMinus -> print_string "-" 
    | TMultiply -> print_string "*" 
    | TDivide -> print_string "/" 
    | TModulo -> print_string "%" 
    | THash -> print_string "#" 
    | ValueENT(k) -> print_int k
    | ValueFLOAT(k) -> print_float k
    | ValueBOOL(k) -> print_string (if (k = true) then "true" else "false")
    | Identifier(s) -> print_string s

  (* Genére le token correspondant à la string donnée *)
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
    | "||" -> TOr
    | "&&" -> TAnd
    | ">" -> TGtn
    | "<" -> TLtn 
    | ">=" -> TGeq 
    | "<=" -> TLeq
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
    | _ -> Identifier(tokenStr)

    (* Parse n'importe quel type de token
       Pour éviter les "mélanges" entre token similaire 
       (Ex : ">=" et ">" ), on explicite certaines régles *)
    let p_token : (token, char) ranalist = fun l ->
      l |>
        (p_lettre ++> fun c ->
                      p_alphanums ++>
                        fun la ->
                        return (create_token(string_of_list (fun () -> (Cons(c, la))))))
        +| (p_chiffre ++> fun c ->
                          p_chiffres ++>
                            fun lc ->
                            (return (ValueENT (create_valueENT (string_of_list (fun () -> (Cons(c, lc)))) 0))))
        +| (terminal ':' +> terminal '=' +> return TAffec)
        +| (terminal '|' +> terminal '|' +> return TOr)
        +| (terminal '&' +> terminal '&' +> return TAnd)
        +| (terminal '>' +> terminal '=' +> return TGeq)
        +| (terminal '<' +> terminal '=' +> return TLeq)
        +| (anyterminal ++> fun c -> return (create_token (string_of_list (fun () -> (Cons(c, (fun () -> Nil)))))))

    (* Affiche la liste des token comme un string *)
  let rec print_token_list = fun list ->
    match list() with
    | Cons(t, suite) -> print_token t; print_token_list suite
    | _ -> print_string "\n"

  exception NotEntValue of string

  (* Parse les char "vide" *)
  let p_espace : char analist = terminal ' ' +| terminal '\n' +| terminal '\t'
  let rec p_espaces : char analist = fun l -> l |>
                                                p_espace +> p_espaces +| epsilon

  let next_token : (token, char) ranalist =
    p_espaces +> p_token

  (* Parse la mylist entière pour genérer tout les tokens *)
  let rec p_tokens : (token mylist, char) ranalist = fun l ->
    l |>
      (next_token ++> fun t -> p_tokens ++> fun lt -> return (fun () -> Cons(t, lt)))
      +| return (fun () -> Nil)


  (* Effectue l'analyse lexicale *)
  exception Echec_lex
  let analex s =
    let t, l = p_tokens (list_of_string s) in
    if l() = Nil then t else raise Echec_lex

  

  
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
