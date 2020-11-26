module AnalyseurLexicale :
sig
  type token

  type 'a mylist

  val print_token : token -> token
  val print_token_list : token mylist -> token

  val consumeBlank : 't mylist -> 't mylist
  val generate_substring : 't mylist -> string -> (string * 't mylist)

  val create_valueENT : string -> int
  val create_valueFLOAT : string -> float
  val checkIdentifier : string -> string
  val create_token : 't mylist -> token
  val token_generator : 't mylist -> token mylist
  
  val list_of_string : string -> 't mylist
end
