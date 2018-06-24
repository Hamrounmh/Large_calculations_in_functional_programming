(************PROJET OCAML *****************)


(***************PARTIE 1 ******************)
#load "str.cma";;

type 'a expr = Multi of 'a expr * 'a expr | Sub of 'a expr * 'a expr | Addi  of 'a expr * 'a expr | Div of 'a expr * 'a expr | Valeur of 'a ;;

type nombre = {valeur : int ; exp :int expr } ;;
let a ={valeur=5 ; exp= Valeur 5 } ;;
let b = {valeur=11 ; exp=Addi (Multi(Valeur 1 , Valeur 3 ), Addi(Valeur 2,Valeur 6))} ;;

(*****Fonction de conversion pour un string ****) 
let  nombre_toString = fun a -> 
  let rec aux = fun a res -> 
    match a with 
      |Addi(x,y) -> res^( aux x "("^((aux y "+"))^")" )
      |Valeur(x)->res^(string_of_int x ) 
      |Multi(x,y)->res^( aux x "("^((aux y "×"))^")" )
      |Sub(x,y)->res^( aux x "("^((aux y "-"))^")" )
      |Div(x,y)->res^( aux x "("^((aux y "÷"))^")" )

  in aux a.exp (string_of_int (a.valeur)^",") ;;


nombre_toString b;;

(*****1- les fonction pour la combinaison ******) 


let rec get_liste_paires = fun liste ->
  match liste with 
      [] -> []
    | x::t -> List.map (function y -> (x,y)) t @ get_liste_paires t;;
(*** cette fonction renvois des paires combinés d'une liste ***) 


let rec filtre_paire = fun l (x,y)->
  let rec aux =  fun l res -> 
    match l with 
      |[]->res
      |h::t -> if ((h<>x) && (h<>y)) then aux t (res@[h]) else 
            aux t res in
    aux l [] ;;
(******* fonction finale pour la combinaison ***)


let combinaison = fun l ->
  let paire = get_liste_paires l 
  in 
    List.map (fun (x,y)-> (x,y,(filtre_paire l (x,y)))) paire ;;


combinaison [1;2;3;4];;


(*** 2eme question les fonction addition , multiplication , sous*****)
(**** partie 1 les fonction pour la verification ***)

let from_toString = fun s ->
  let rec aux = fun s res ->
    let l=String.length s in
      if l=0 then res 
      else aux (String.sub s 1 (l-1)) res@[(String.sub s 0 1)]
  in aux s [];;



let int_string = fun l ->
  let rec aux = fun l res ->
    match l with 
      |[]->res
      |h::t-> try aux t res@[(int_of_string h)] with
          _-> aux t res 
  in aux l [];;

int_string (from_toString "((3+2)-1)*2)");;

let nb_occ= fun l n ->
  let rec aux = fun l ->
    match l with 
      |[]->0
      |h::t -> if h=n then 1+ aux t 
          else aux t 
  in aux l ;;
nb_occ (int_string (from_toString "((3+2)-1)*2)")) 2;;

let rec inclus = fun l k ->
  match l with 
    |[]-> true
    |h::t ->if(nb_occ l h)<=(nb_occ k h) then inclus t k 
        else false ;;
let egal l l' = inclus l l' && inclus l' l ;;

egal [1;2;3;5] [5;2;3;1] ;;
let equals = fun a b ->
  let l1=int_string(from_toString (nombre_toString a))
  in
  let l2=int_string(from_toString (nombre_toString b))
  in
    egal l1 l2 ;;
let n1={valeur = 2; exp = Div(Valeur 4,Valeur 2) } ;;
let n2={valeur = 2 ; exp = Sub(Valeur 4,Valeur 2) } ;;

equals n1 n2;;

(*fonction de verification des expression avec des fonctions auxiliaires *)
let verifier = fun a l ->
  let rec aux = fun l ->
    match l with 
      |[]->false
      |h::t-> 
          begin 
            let rec aux'= fun h ->
              match h with 
                |[] -> aux t 
                |z::s->if (equals z a) then true 
                    else aux' s
            in aux' h 
          end 
  in aux l ;;
let verifier1= fun a l ->
  let rec aux = fun l ->
    match l with 
      |[]->false
      |h::t-> if (equals h a ) then true else aux t 
  in aux l ;;


(****** la fonction verifier1 verifier une expression avec une liste d'expression *)
(********************partie 2 : les fonction addition , multiplication , soustration,division******)

let addition = fun a b l -> 
  let nb=b.valeur and na=a.valeur in 
    match na,nb with 
      |(x,0)->[a]::[l]
      |(0,x)->[b]::[l]
      |(x,y)->let c={valeur=(x+y);exp= (Addi(a.exp,b.exp))}

          in if verifier1 c l then [l]
            else if c.valeur <= 0 then [l] 
            else [c]::[l];;


let soustraction = fun a b l -> 
  let nb=b.valeur and na=a.valeur in 
    match na,nb with 
      |(x,0)->if x<=0 then [l] else [a]::[l]
      |(0,x)->if x<=0 then [l] else [b]::[l]
      |(x,y)-> if x<y then let c={valeur=(y-x);exp= (Sub(b.exp,a.exp))} 
            in if verifier1 c l then [l]
              else if c.valeur <= 0 then [l] 
              else [c]::[l]
          else let c={valeur=(x-y);exp= (Sub(a.exp,b.exp))} 
            in if verifier1 c l then [l]
              else if c.valeur <= 0 then [l] 
              else [c]::[l];;

let multiplication = fun a b l -> 
  let nb=b.valeur and na=a.valeur in 
    match na,nb with 
      |(x,0)-> [l]
      |(0,x)-> [l]
      |(1,x)->[l]
      |(x,1)->[l]
      |(x,y)-> let c={valeur=(x*y);exp= (Multi(b.exp,a.exp))} 
          in if verifier1 c l then [l]
            else if c.valeur <= 0 then [l] 
            else [c]::[l];;

let division n1 n2 l = 
  let a = n1.valeur and b = n2.valeur 
  in match a,b with
    |(n,0) -> [l]
    |(0,n) -> [l]
    |(n,m) -> if n > m && n mod m = 0 then let c = { valeur = (n/m); exp = Div(n1.exp,n2.exp) } in
            if (verifier1 c l) || c.valeur <= 0 then [l] else [c]::[l]
        else if m mod n = 0 then let c = { valeur = (m/n); exp = Div(n2.exp,n1.exp) }
          in if (verifier1 c l ) || c.valeur <= 0 then [l] else [c]::[l]
        else [l];;



(*partie 1 , question 2 "construction des combinaison " ***)
let fus l=(List.hd(l))@(if (List.length(List.tl l)!=0) then  List.hd(List.tl l) else []) ;;

let rec grand = fun l ->
  let petit = fun c ->
    match c with 
      |(a,b,[])->addition a b [] @ multiplication a b [] @ division a b [] @ soustraction a b [] 
      |(a,b,l) -> grand (fus (addition a b l)) @ grand (fus (soustraction a b l)) @ grand (fus (multiplication  a b l)) @ grand (fus (division  a b l)) 
  in
  let com= combinaison l 
  in 
  let rec aux = fun com -> 
    match com with 
      |[]->[]
      |h::t->(petit h) @ (aux t)
  in aux com 
;;

let nom x = {valeur=x;exp=Valeur x};;


let rec transpose lists =
  match lists with
      [] -> []
    |h::t -> h @ transpose t;;

let x = transpose (grand [nom 1;nom 2;nom 3]);;

let rec filtrage = fun l -> 
  match l with 
    |[]->[] 
    |h::t-> if not (verifier1 h t) then h::(filtrage t) else 
          filtrage t;;





(*********"                PARTIE 2                         "*****)

let abs n = if n<0 then -n else n;;
let proche = fun a n1 n2 ->
  if abs (a-n1.valeur) <= abs (a-n2.valeur) then n1 else n2 ;;


let rec meilleure_approximation = fun a l ->
  match l with
    |[]-> failwith "faux" 
    |h1::h2::t->meilleure_approximation a ((proche a h1 h2)::t)
    |h::[]->h
;;


let meilleur_approximation a l= meilleure_approximation a (transpose (grand l));;



meilleur_approximation 15 [nom 60; nom 6;nom 1];;

(*****************"         PARTIE 3            "****************) 



let intlist_nombre l = List.map (fun x ->nom x) l;;





exception Pas_de_solution;;


let meilleure_solution = fun a l ->
  let b = meilleur_approximation a l in if b.valeur = a then b else raise Pas_de_solution ;;

let to_string =
  let rec p l r = function
    | Valeur x -> string_of_int x
    | Addi (x, y) -> Printf.sprintf "%s%s + %s%s" l (inner x) (inner y) r
    | Sub (x, y) -> Printf.sprintf "%s%s - %s%s" l (inner x) (inner y) r
    | Multi (x, y) -> Printf.sprintf "%s%s * %s%s" l (inner x) (inner y) r
    | Div (x, y) -> Printf.sprintf "%s%s / %s%s" l (inner x) (inner y) r
  and inner expr = p "(" ")" expr in p "" "";;






let to_string_solution =fun {valeur=valeur; exp=exp}->
  Printf.printf "une valeur = %d  correspondant a l'expression  \" %s \"" valeur (to_string exp);print_newline();;




let afficher_combinaison l=
  let liste= filtrage (transpose (grand l))
  in
  let rec aux l=
    match l with
      |[]->  print_string ""
      |h::t -> (to_string_solution h) ;aux t
  in aux (liste) ;;




let exact = fun a l  -> 
  let b =meilleure_solution a l in if b.valeur = a then b else raise Pas_de_solution;;


let transformer_liststring_to_int s =List.map int_of_string (Str.split (Str.regexp "[^0-9]+") s);;


exception Trop_grande;;
exception Choixincorrect ;;



let choix = fun a ->
  if String.length a > 0 
  then 
    match a with 
      |"1" -> 
          begin print_string " donnez une liste d'entrer "; 
            let s = read_line() in
            let l = intlist_nombre (transformer_liststring_to_int s) in
              if List.length l > 11 then raise Trop_grande else 
                afficher_combinaison (l) 
          end 
      |"2" -> begin 
          print_string "donnez un entier a calculer :" ;
          let b = (int_of_string(read_line())) in
            print_string " donnez la liste de depart :" ;
            let s = read_line() in
            let l = intlist_nombre (transformer_liststring_to_int s) in
              to_string_solution (meilleur_approximation b l )
        end
      |"3"-> begin 
          print_string "donnez un entier a calculer :";
          let b =(int_of_string(read_line())) in
            print_string"donnez la liste de depart :";
            let s = read_line() in
            let l = intlist_nombre (transformer_liststring_to_int s) in
              if ((meilleur_approximation b l).valeur= b) then  to_string_solution ((meilleur_approximation b l)) else
                print_string "valeur non trouve ";print_string " "
        end


      |"4" -> exit(0)
      |_-> raise Choixincorrect 
  else raise Choixincorrect ;;
let main () =
  let rec aux () = 
    print_string " ~~~~ Menu ~~~~\n 
**********       1 - Afficher les combinaisons            **********\n
**********       2 - Afficher la meilleure combinaison    **********\n
**********       3 - Afficher le meilleure resultat       **********\n
**********       4 - sortir                               **********\n
" ; 
    let a = read_line() 
    in try 
        (choix a) ; aux ()
      with 
        |Pas_de_solution -> print_string "Aucune solution trouve " ; aux ()
        |Trop_grande -> print_string "  La liste de depart est trop grande " ; aux ()
        |Choixincorrect -> print_string " Votre choix est incorrect  " ;aux ()
  in aux () ;;
main ();;







