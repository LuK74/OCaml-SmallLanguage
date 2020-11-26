open State

module S = State

module ArithExp =
  struct

    type aExp =
      | CoENT of int
      | VaENT of string
      | AplENT of aExp * aExp
      | AmuENT of aExp * aExp
      | AmiENT of aExp * aExp
      | AdiENT of aExp * aExp
      | AmoENT of aExp * aExp

    exception TypeError

    let rec evalAExp = fun a1 ->
      fun s1 ->
      match a1 with
      | CoENT (k) -> k
      | VaENT (k) -> (let res = S.read_var k s1 in
                     (match res with
                     | S.VEnt(k) -> k
                     | _ -> raise TypeError))
      | AplENT (a1 , a2) -> (evalAExp a1 s1) + (evalAExp a2 s1)
      | AmuENT (a1 , a2) -> (evalAExp a1 s1) * (evalAExp a2 s1)
      | AmiENT (a1 , a2) -> (evalAExp a1 s1) - (evalAExp a2 s1)
      | AdiENT (a1 , a2) -> (evalAExp a1 s1) / (evalAExp a2 s1)
      | AmoENT (a1 , a2) -> (evalAExp a1 s1) mod (evalAExp a2 s1)
    
    (*type aExp = AExpENT of aExpENT | AExpFLOAT of aExpFLOAT
    and aExpENT =
      | CoENT of int
      | VaENT of string
      | AplENT of aExp * aExp
      | AmuENT of aExp * aExp
      | AmiENT of aExp * aExp
      | AdiENT of aExp * aExp
      | AmoENT of aExp * aExp
    and aExpFLOAT =
      | CoFLOAT of float
      | VaFLOAT of string
      | AplFLOAT of aExp * aExp
      | AmuFLOAT of aExp * aExp
      | AmiFLOAT of aExp * aExp
      | AdiFLOAT of aExp * aExp
      | AmoFLOAT of aExp * aExp*)


    (*let rec evalAExp = fun a1 ->
      fun s1 ->
      match a1 with
      | AExpENT (ae1) -> evalAExpENT ae1 s1
      | AExpFLOAT (af1) -> evalAExpFLOAT af1 s1
    and evalAExpFLOAT = fun a1 ->
      fun s1 ->
      match a1 with
      | CoFLOAT (k) -> k
      | VaFLOAT (k) -> S.read_var s1 k
      | AplFLOAT (a1 , a2) -> (evalAExp) a1 s1 + (evalAExp) a2 s1
      | AmuFLOAT (a1 , a2) -> (evalAExp) a1 s1 * (evalAExp) a2 s1
      | AmiFLOAT (a1 , a2) -> (evalAExp) a1 s1 - (evalAExp) a2 s1
      | AdiFLOAT (a1 , a2) -> (evalAExp) a1 s1 / (evalAExp) a2 s1
      | AmoFLOAT (a1 , a2) -> (evalAExp) a1 s1 mod (evalAExp) a2 s1
    and evalAExpENT = fun a1 ->
      fun s1 ->
      match a1 with
      | CoENT (k) -> k
      | VaENT (k) -> S.read_var s1 k
      | AplENT (a1 , a2) -> (evalAExp) a1 s1 + (evalAExp) a2 s1
      | AmuENT (a1 , a2) -> (evalAExp) a1 s1 * (evalAExp) a2 s1
      | AmiENT (a1 , a2) -> (evalAExp) a1 s1 - (evalAExp) a2 s1
      | AdiENT (a1 , a2) -> (evalAExp) a1 s1 / (evalAExp) a2 s1
      | AmoENT (a1 , a2) -> (evalAExp) a1 s1 mod (evalAExp) a2 s1*)

  end
