(*typingg27.ml*)

type ide = string ;;

		 
type eval = None 
          | Int of int 
          | Bool of bool
		  | Char of char ;;

		  
type exp = Val of ide
			| Eint of int
			| Echar of char
			| True
			| False
			| Empty
			| Sum of exp * exp
			| Diff of exp * exp
			| Times of exp * exp
			| And of exp * exp
			| Or of exp * exp
			| Not of exp
			| Eq of exp * exp
			| Less of exp * exp
			| Cons of exp * exp
			| Head of exp
			| Tail of exp(*da fare nella semantica*)
			| Fst of exp
			| Snd of exp
			| Epair of exp * exp(*da fare nella semantica*)
			(*ahahahahah queste funzioni sono da capire e vedere come implementarle*)
			| Ifthenelse of exp * exp * exp
			| Let of ide * exp * exp
			| Fun of ide * exp
			| Appl of exp * exp
			| Rec of ide * exp ;;
			
type env = ide -> eval;;

		  
type eval = Undefined
			| Int of int
			| Bool of bool
			| Char of char
			| List of eval list
			| Pair of eval * eval
			| Closure of exp * env ;;
			
			
type etype = TBool
			| TInt
			| Tchar
			| TVar of string
			| TPair of etype * etype
			| TList of etype list
			| TFun of etype * etype;;

		