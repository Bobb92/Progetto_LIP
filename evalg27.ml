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

let typecheck (x, y) = match x with
						| "int" -> (match y with 
									|  Int(u) -> true
									| _ -> false)
						| "bool" -> (match y with 
									|  Bool(u) -> true
									| _ -> false)
						| "char" -> (match y with
									| Char(u) -> true
									| _ -> false)
						| "liste" -> (match y with
									| List(u) -> true
									| _ -> false)
						| "pair" -> (match y with
									| Pair(u) -> true
									| _ -> false)
						| _ -> failwith ("not a valid type");;
	  
let eq a = match a with
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
		
let sum a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u + w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let diff a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u - w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let times a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Int(u * w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let anda a = match a with 
			| (x,y) -> if typecheck("bool",x) & typecheck("bool",y) 
					then 
						(match (x,y) with
						| (Bool(u), Bool(w)) -> Bool(u & w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;
		
let ora a = match a with 
			| (x,y) -> if typecheck("bool",x) & typecheck("bool",y) 
					then 
						(match (x,y) with
						| (Bool(u), Bool(w)) -> Bool(u | w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;

let not a = if typecheck("bool", a) 
			then (if a then Bool(false) else Bool(true))
			else failwith ("type error");;
			
let less a = match a with 
			| (x,y) -> if typecheck("int",x) & typecheck("int",y) 
					then 
						(match (x,y) with
						| (Int(u), Int(w)) -> Bool(u < w))
				  else failwith ("type error")
		| _ -> failwith ("argument error");;

let head a = if typecheck("list",a) 
			then
				match a with
				| x::xl -> x
				| x::[] -> x
				| _ -> failwith ("invalid list")
			else failwith ("type error");;
			
let fst a = match a with 
			| (x,y) -> x
			| _ -> failwith ("invalid tupla");;
			
let snd a = match a with 
			| (x,y) -> y
			| _ -> failwith ("invalid tupla");;
			
let cons a = match a with
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
			
let tail a = match a with
			| x::xl -> xl
			| _ -> failwith ("invalid list");;
			
let epair a =  match a with
			| (x,y) -> (x,y)
			| _ -> failwith ("invalid tupla");;
			
let ifthenelse a = match a with 
				  | (x,y,z) -> if typecheck("bool",x) 
							   then if typecheck("bool",y) & typecheck("bool",z)
									then if x then y else z
									else if typecheck("int",y) & typecheck("int",z)
										 then if x then y else z
										 else if typecheck("char",y) & typecheck("char",z)
											  then if x then y else z
											  else if typecheck("list",y) & typecheck("list",z)
												  then if x then y else z
												  else if typecheck("pair",y) & typecheck("pair",z)
													  then if x then y else z
													  else failwith("type return error")
							   else failwith ("type error")
				  | _ -> failwith ("type input error");;
				  
let rec sem (e:exp) (r:env) = match e with (*e è l'espressione da valutare,  r è l'ambiente*)
								
							| Eint(n) -> Int(n)
							| Echar(c) -> Char(c)
							| True (b) -> Bool(true)
							| False (b) -> Bool(false)
							| Eq (a,b) -> eq ((sem a r),(sem b r))
							| Sum (a,b) -> sum ((sem a r),(sem b r))
							| Diff (a,b) -> diff ((sem a r),(sem b r))
							| Times (a,b) -> times ((sem a r),(sem b r))
							| And (a,b) -> anda ((sem a r),(sem b r))
							| Or (a,b) -> ora ((sem a r),(sem b r))
							| Not a -> not (sem a r)
							| Less (a,b) -> less ((sem a r),(sem b r))
							| Head (a) -> head (sem a r)
							| Fst (a) -> fst (sem a r)
							| Snd (a) -> snd (sem a r)
							| Cons (a,b) -> cons ((sem a r),(sem b r))
							| Tail (a) -> tail (sem a r)
							| Epair (a,b) -> epair ((sem a r),(sem b r))
							| Ifthenelse (a,b,c) -> ifthenelse ((sem a r),(sem b r),(sem c r));;(*da fare la let, rec e appl*)
							
							




		
		
		

 

	  