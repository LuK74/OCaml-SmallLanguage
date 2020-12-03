open ArithExp
open State

module S = State
module Ar = ArithExp

module BoolExp =
  struct

    (* Ces 3 types représentant les opérateurs d'une expression pouvant renvoyant
       une booléen *)

    (* Opérateur de comparaison entre expressions arithmétiques *)
    type opeBinA =
      | OEqual
      | OLeq
      | OGeq
      | OGtn
      | OLtn

    (* Opérateur booléen binaire *)
    type opeBinB =
      | OOr
      | OAnd

    (* Opérateur booléen unaire *)
    type opeUnaB =
      | ONeg

    (* CoB : constante booléenne
       VaB : variable booléenne 
       BinOpB, BinOpA, UnaOpB décrit au-dessus *)
    type boolExp =
      | CoB of bool
      | VaB of string
      | BinOpB of opeBinB * boolExp * boolExp
      | BinOpA of opeBinA * Ar.aExp * Ar.aExp
      | UnaOpB of opeUnaB * boolExp
    
    exception TypeError
    exception WrongPrintFunc
    
    
    let rec printBExp = fun b1 ->
      match b1 with
      | CoB (k) -> if (k = true) then print_string "true" else print_string "else"
      | VaB (k) -> print_string k
      | BinOpB(_,_,_) -> printBinOpB b1
      | BinOpA(_,_,_) -> printBinOpA b1
      | UnaOpB(_,_) -> printUnaOpB b1
    and printBinOpB = fun b ->
      match b with
      | BinOpB(op, b1, b2) ->
         (match op with
          | OOr ->  printBExp b1; print_string "||"; printBExp b2
          | OAnd -> printBExp b1; print_string "&&"; printBExp b2)
      | _ -> raise WrongPrintFunc
    and printUnaOpB = fun b ->
      match b with
      | UnaOpB (op, b1) ->
         (match op with
          | ONeg -> print_char '!';printBExp b1)
      | _ -> raise WrongPrintFunc
    and printBinOpA = fun b ->
      match b with
      | BinOpA(op, aENT1, aENT2) ->
         (match op with
          | OEqual -> Ar.printAExp aENT1; print_char '='; Ar.printAExp aENT2
          | OLeq -> Ar.printAExp aENT1; print_string "<="; Ar.printAExp aENT2
          | OGeq -> Ar.printAExp aENT1; print_string ">="; Ar.printAExp aENT2
          | OGtn -> Ar.printAExp aENT1; print_char '>'; Ar.printAExp aENT2
          | OLtn -> Ar.printAExp aENT1; print_char '<'; Ar.printAExp aENT2)
      | _ -> raise WrongPrintFunc


    (* Evaluation d'une expression booléenne *)
    let rec evalBExp = fun b1 -> fun s1 ->
      (match b1 with
      | CoB (k) -> k
      | VaB (k) -> let evalBRes = S.read_var k s1 in
                   (match evalBRes with
                    | S.VBool(k) -> k
                    | _ -> raise TypeError)
      | BinOpB(_,_,_) -> evalBinOpB b1 s1
      | BinOpA(_,_,_) -> evalBinOpA b1 s1
      | UnaOpB(_,_) -> evalUnaOpB b1 s1)
    and evalBinOpB = fun b -> fun s1 ->
      (match b with
      | BinOpB(op, b1, b2) ->
         (match op with
          | OOr -> ((evalBExp b1 s1) || (evalBExp b2 s1))
          | OAnd ->  ((evalBExp b1 s1) && (evalBExp b2 s1)))
      | _ -> raise WrongPrintFunc)
    and evalUnaOpB = fun b -> fun s1 ->
      (match b with
      | UnaOpB (op, b1) ->
         (match op with
          | ONeg ->  if ((evalBExp b1 s1) = true) then false else true)
      | _ -> raise WrongPrintFunc)
    and evalBinOpA = fun b -> fun s1 ->
      (match b with
      | BinOpA(op, aENT1, aENT2) ->
         (match op with
          | OEqual -> if ((Ar.evalAExp aENT1 s1) = (Ar.evalAExp aENT2 s1)) then true else false
          | OLeq ->  if ((Ar.evalAExp aENT1 s1) <= (Ar.evalAExp aENT2 s1)) then true else false
          | OGeq -> if ((Ar.evalAExp aENT1 s1) >= (Ar.evalAExp aENT2 s1)) then true
                    else false
          | OGtn ->if ((Ar.evalAExp aENT1 s1) > (Ar.evalAExp aENT2 s1)) then true
                   else false
          | OLtn ->  if ((Ar.evalAExp aENT1 s1) < (Ar.evalAExp aENT2 s1)) then true
                     else false)
      | _ -> raise WrongPrintFunc)
    
    


  end
