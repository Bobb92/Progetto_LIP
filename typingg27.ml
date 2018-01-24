(*typingg27.ml*)

Type ide = string;;

		 
type eval = None 
          | Int of int 
          | Bool of bool

		  
type exp = 
			Eint of int 
		  | Ebool of bool 
		  | Den of ide
		  | Prod of exp * exp
		  | Sum of exp * exp
		  | Diff of exp * exp
		  | Eq of exp * exp
		  | Minus of exp
		  | Iszero of exp
		  | Or of exp * exp
		  | And of exp * exp
		  | Not of exp
		  | Ifthenelse of exp * exp * exp;;
		  
Type exp = X_a of eval
		  |Plus of exp * exp
		  |Menus of exp * exp
		  |Mul of exp * exp 
		  |Not of exp * exp 
		  |Equals of exp * exp
		  |Less of exp * exp
		  |Hig of exp * exp
		  |Or of exp * exp
		  |And of exp * exp
		  |Var of var
		  |Let of var * exp * exp
		  |Liste of exp::exp
		  |Tupla of  epx * exp
		  |Fun_a of exp(exp)
		  |Not_a of exp;;
		