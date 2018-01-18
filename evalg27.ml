(* Operations on Eval *)
(*Sto coppiando cose da dokuwiki per realizzare una base per il progetto, questa è la parte 2*)

let typecheck (x, y) = match x with
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
	  
 
    let minus x =
      if typecheck("int",x) 
      then 
    	(match x with
    	| Int(y) -> Int(-y) )
      else 
    	failwith ("type error")
 
    let iszero x =
      if typecheck("int",x) 
      then 
     	(match x with
     	| Int(y) -> Bool(y=0) )
      else 
     	failwith ("type error")
 
    let equ (x,y) =
      if typecheck("int",x) & typecheck("int",y) 
      then 
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Bool(u = w))
      else failwith ("type error")
 
    let plus (x,y) =
      if typecheck("int",x) & typecheck("int",y) 
      then 
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u+w))
      else failwith ("type error")
 
    let diff (x,y) =
      if typecheck("int",x) & typecheck("int",y) 
      then 
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u-w))
      else failwith ("type error")
 
    let mult (x,y) =
      if typecheck("int",x) & typecheck("int",y) 
      then 
   	(match (x,y) with
   	| (Int(u), Int(w)) -> Int(u*w))
      else failwith ("type error")
 
    let et (x,y) =
      if typecheck("bool",x) & typecheck("bool",y) 
      then 
   	(match (x,y) with
   	| (Bool(u), Bool(w)) -> Bool(u & w))
      else failwith ("type error")
 
    let vel (x,y) =
      if typecheck("bool",x) & typecheck("bool",y) 
      then 
   	(match (x,y) with
   	| (Bool(u), Bool(w)) -> Bool(u or w))
      else failwith ("type error")
 
    let non x =
      if typecheck("bool",x) 
      then 
     	(match x with
     	| Bool(y) -> Bool(not y) )
      else failwith ("type error");;