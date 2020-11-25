module AnalyseurSyntaxique :
sig
  (* Type utilisé pour les token de l'AST *)
  type var
  type const
  type exp
  type ope
  type instr

  val print_instr : instr

  (* Type utilisé pour l'analyse syntaxique *)
  exception Echec
  type 'a mylist
  type ('r, 't) ranalist
  type ('t) analist
  type ('x, 't) st

  val (+>) : (('t) analist) -> (('x, 't) st) -> ('x, 't) st
  val (++>) : (('r, 't) ranalist) -> ('r -> ('x, 't) st) -> ('x, 't) st
  val (+|) : (('x, 't) st) -> (('x, 't) st) -> ('x, 't) st

  val terminal : 't -> 't mylist -> 't mylist
  val return : 'r -> 't mylist -> ('r * 't mylist)

  val p_V : (var, 't) ranalist
  val p_C : (const, 't) ranalist
  val p_E : (exp ,'t) ranalist
  val p_O : (ope, 't) ranalist

  val p_If : 't -> 't mylist -> 't mylist
  val p_While : 't -> 't mylist -> 't mylist
  
  val p_S : (instr, 't) ranalist
  val p_L : (instr, 't) ranalist
  val p_I : (instr, 't) ranalist

                        (* useful func for parsing *)
  val list_of_string : string -> 't mylist
  val ast_parser_func : 't mylist -> 'r
  
  

end
