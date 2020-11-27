open ArithExp
open State

module S = State
module Ar = ArithExp

module BoolExp =
  struct
    type boolExp =
      | CoB of bool
      | VaB of string
      | OrB of boolExp * boolExp
      | AndB of boolExp * boolExp
      | NegB of boolExp
      | EqualB of Ar.aExp * Ar.aExp

    exception TypeError

     let rec printBExp = fun b1 ->
      match b1 with
      | CoB (k) -> if (k = true) then print_string "true" else print_string "else"
      | VaB (k) -> print_string k
      | OrB (b1, b2) -> printBExp b1; print_string "||"; printBExp b2
      | AndB (b1, b2) -> printBExp b1; print_string "&&"; printBExp b2
      | NegB (b1) -> print_char '!';printBExp b1
      | EqualB (aENT1, aENT2) -> Ar.printAExp aENT1; print_char '='; Ar.printAExp aENT2

    let rec evalBExp = fun b1 ->
      fun s1 ->
      match b1 with
      | CoB (k) -> k
      | VaB (k) -> (let res = S.read_var k s1 in
                    (match res with
                     | S.VBool(k) -> k
                     | _ -> raise TypeError))
      | OrB (b1, b2) -> (evalBExp b1 s1 || evalBExp b2 s1)
      | AndB (b1, b2) -> (evalBExp b1 s1 && evalBExp b2 s1)
      | NegB (b1) -> if ((evalBExp b1 s1) = true) then true else false
      | EqualB (aENT1, aENT2) -> if ((Ar.evalAExp aENT1 s1) = (Ar.evalAExp aENT2 s1)) then true
                                    else false

  end
