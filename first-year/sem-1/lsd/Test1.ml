(*1*)

let rec f1 x = if (x>=12) then x-1 else f1(f1(x+2))

(*2*)

let  removeAt n l=
  let rec removeAtint rez k n l = match l with
  | [] -> List.rev rez
  | h::t when k=n -> removeAtint rez (k+1) n t
  | h::t -> removeAtint (h::rez) (k+1) n t 
  in removeAtint [] 0 n l;;

  let r = removeAt 2 [1;2;3;4;5]

(*3*)

let faux x= if (x mod 2 = 0) then true else false

let filtru_invers f l=List.fold_left(fun a b -> if f b  then a else b::a) [] l

;;