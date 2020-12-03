open AnalyseurLexicale
open BoolExp
open ArithExp

module AL = AnalyseurLexicale
module Bo = BoolExp
module Ar = ArithExp

module AnalyseurSyntaxique :
sig
  (* Type utilisé pour les token de l'AST *)
  type var
  type exp
  
  type instr

  val print_instr : instr

  (* Type utilisé pour l'analyse syntaxique *)
  type ('r, 't) ranalist
  type ('t) analist
  type ('x, 't) st

  val (+>) : (('t) analist) -> (('x, 't) st) -> ('x, 't) st
  val (++>) : (('r, 't) ranalist) -> ('r -> ('x, 't) st) -> ('x, 't) st
  val (+|) : (('x, 't) st) -> (('x, 't) st) -> ('x, 't) st

  val terminal : 't -> 't AL.mylist -> 't AL.mylist
  val return : 'r -> 't AL.mylist -> ('r * 't AL.mylist)

  val p_V : (var, AL.token) ranalist
  val p_C : (exp, AL.token) ranalist
  val p_E : (exp, AL.token) ranalist

  val p_OpeA : (Ar.ope, AL.token) ranalist
  val p_OpeBinB : (Bo.opeBinB, AL.token) ranalist
  val p_OpeBinA : (Bo.opeBinA, AL.token) ranalist
  val p_OpeUnaB : (Bo.opeUnaB, AL.token) ranalist
  
  val p_S : (instr, 't) ranalist
  val p_L : (instr, 't) ranalist
  val p_I : (instr, 't) ranalist

  val p_AExp : (exp, AL.token) ranalist
  val p_BExp : (exp, AL.token) ranalist
  val p_T : (exp, AL.token) ranalist
  val p_Exp : (exp, AL.token) ranalist
  
  (* useful func for parsing *)
  val list_of_string : string -> 't AL.mylist
  val ast_parser_func : 't AL.mylist -> 'r
  
  

end
