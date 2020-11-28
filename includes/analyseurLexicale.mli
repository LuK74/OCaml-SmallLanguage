module AnalyseurLexicale :
sig
  type token
  type 'a mylist

  (* utils func *)
  val valueChar : char -> int

  val isNumb : char -> bool
  val isChar : char -> bool

  val create_valueENT : string -> int
  val create_valueFLOAT : string -> float
  
  val print_token : token -> token
  val print_token_list : token mylist -> token


  (* parsing func *)
  type 't analist
  type ('r, 't) ranalist

  val terminal : char -> 't mylist -> 't mylist
  val anyterminal : char -> 't mylist -> ('r * 't mylist)

  val epsilon : 't analist
  val return : ('r, 't) ranalist

  val (+>) : 't mylist -> ('t analist) -> ('t mylist -> 'x) -> 'x
  val (++>) : 't mylist -> (('r, 't) ranalist) -> ('r -> 't mylist -> 'x) -> 'x
  val (+|) : 't mylist -> ('t mylist -> 'x) -> ('t mylist -> 'x) -> 'x
  

  val analex : string -> token mylist
  

end
