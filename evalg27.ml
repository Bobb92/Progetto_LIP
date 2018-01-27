(* Operations on Eval *)

type ide = Ide of string;;

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
			| Tail of exp
			| Fst of exp
			| Snd of exp
			| Epair of exp * exp
			| Ifthenelse of exp * exp * exp
			| Let of ide * exp * exp
			| Fun of ide * exp
			| Appl of exp * exp
			| Rec of ide * exp ;;

type eval =
			Undefined
			| Int of int
			| Bool of bool
			| Char of char
			| List of eval list
			| Pair of eval * eval
			| Closure of exp * env
			and
			env = ide -> eval;;

let typecheck (x, y) =
      match x with
      | "int" ->
    (match y with 
    |  Int(u) -> true
    | _ -> false)
      | "bool" ->
    (match y with 
    |  Bool(u) -> true
    | _ -> false)
      | "char" -> 
          (match y with
          | Char(u) -> true
          | _ -> false)
      | _ -> failwith ("not a valid type");;
	  
let Eq a = match a with
		| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Bool(u = w))
				  else if typecheck("char",x) & typecheck("char",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Bool(u = w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let Sum a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u + w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let Diff a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u - w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let Times a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u * w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let And a = match a with 
			| (x,y) -> if typecheck("bool",x) & typecheck("bool",y) 
					then 
						(match (x,y) with
						| (Bool(u), Bool(w)) -> Bool(u & w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let Or a = match a with 
			| (x,y) -> if typecheck("bool",x) & typecheck("bool",y) 
					then 
						(match (x,y) with
						| (Bool(u), Bool(w)) -> Bool(u | w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;

let Not a = if typecheck("bool", a) 
			then (if a then Bool(false) else Bool(true))
			else failwith ("type error");;
			
let Less a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Bool(u < w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;

let Head a = match a with
			| x::xl -> x
			| _ -> failwith ("invalid list");;
			
let Fst a = match a with 
			| (x,y) -> x
			| _ -> failwith ("invalid tupla");;
			
let Snd a = match a with 
			| (x,y) -> y
			| _ -> failwith ("invalid tupla");;
			
let Cons a = match a with
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int list(u :: w))
				  else if typecheck("bool",x) & typecheck("bool",y) 
					then 
						(match (x,y) with
						| (Bool(u), Bool(w)) -> Bool list(u :: w))
				  else if typecheck("char",x) & typecheck("char",y) 
					then 
						(match (x,y) with
						| (Char(u), Char(w)) -> Char list(u :: w))
				  else failwith ("type error")
			| _ -> failwith ("invalid ");;
			
let Tail a = match a with
			| x::xl -> xl
			| _ -> failwith ("invalid list");;
			
let Epair a =  match a with
			| (x,y) -> (x,y)
			| _ -> failwith ("invalid tupla");;



		
		
		

 

	  