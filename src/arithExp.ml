open State
open AnalyseurLexicale

module S = State
module AL = AnalyseurLexicale

module ArithExp =
  struct

    (* Opérateur binaire entre expression arithmétique *)
    type ope =
      | APlus
      | AMinus
      | ADivide
      | AMultiply
      | AModulo
    
    type aExp =
      | CoENT of int
      | VaENT of string
      | BinOpENT of ope * aExp * aExp

    exception TypeError

    let rec printAExp = fun a1 ->
      match a1 with
      | CoENT (k) -> print_int k
      | VaENT (k) -> print_string k
      | BinOpENT(op, a1, a2) ->
         (match op with
          | APlus -> printAExp a1; print_char '+'; printAExp a2
          | AMultiply -> printAExp a1; print_char '*'; printAExp a2
          | AMinus -> printAExp a1; print_char '-'; printAExp a2
          | ADivide -> printAExp a1; print_char '/'; printAExp a2
          | AModulo -> printAExp a1; print_char '%'; printAExp a2)

    let rec evalAExp = fun a1 ->
      fun s1 ->
      match a1 with
      | CoENT (k) -> k
      | VaENT (k) -> (let res = S.read_var k s1 in
                     (match res with
                     | S.VEnt(k) -> k
                     | _ -> raise TypeError))
      | BinOpENT(op, a1, a2) ->
         (match op with
          | APlus -> (evalAExp a1 s1) + (evalAExp a2 s1)
          | AMultiply -> (evalAExp a1 s1) * (evalAExp a2 s1)
          | AMinus -> (evalAExp a1 s1) - (evalAExp a2 s1)
          | ADivide -> (evalAExp a1 s1) / (evalAExp a2 s1)
          | AModulo -> (evalAExp a1 s1) mod (evalAExp a2 s1))

  end
