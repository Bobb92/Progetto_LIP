(*typingg27.ml*)

(*realizzare i tipi intero op variabile*)

(*t1 + t2, t1 − t2, t1 × t2, t1 ∧ t2, t1 ∨ t2, t1 = t2,t1 < t2*)

Type var = string of String;;(*da ricordarci di implementare la Let *)

Type op = c
		 piu of char
		|meno of char
		|per of char 
		|not of char 
		|uguale of char
		|minore of char
		| t1 ∧ t2 of bool | t1 ∨ t2 of bool;;
		 
Type x = a | int | bool | char;;

Type exp = x_a of x
		  |op_a of exp * op * exp(*da ricorsarsi implementare l'errore op * op * op (+++) tipo aspettato exp * op * exp *)
		  |var_a of var
		  |let_a of var * exp * exp
		  |liste of exp::expτ 
		  |tupla of  epx * exp
		  | τ1(τ2)
		  |_ (*mettere messaggio di tipo non conosciuto*);;
		