(** QUELQUE FONCTIONS AUXILIÈRES UTILES POUR TOUS LE PROJET (LES DEUX PHASES)**)

(* [split_string s] retourne la liste de caractères correspondant Ã  la chaine [s] *)
let split_string s =
  let rec aux i n =
    if i >= n
    then []
    else (String.get s i)::aux (i+1) n
  in
  aux 0 (String.length s)

(* [parse_input ()] lit deux lignes sur l'entrÃ©e standard du programme. 
   La premiÃ¨re doit comporter un entier, la seconde une chaÃ®ne de caractÃ¨res. 
   Retourne un couple composÃ© de l'entier de la première ligne et de la liste des caractÃ¨res de la seconde.
   
   LÃ¨ve l'exception [Failure "int_of_string"] si la première ligne ne reprÃ©sente pas un entier.
 *)
let parse_input () =
  let nb_antennas = int_of_string (read_line ()) in
  let phrase = read_line () in
  nb_antennas,split_string phrase

(*
    @requires: prend un caractère MAJUSCULE de A à Z.
    @ensures : cette fonction va prendre un caractère et renvoie l'indice correspondant (A a pour indice 1, B est pour 2 est ainsi de suite).
	           sinon

*)
let char_index = fun c ->
	
	if c == ' ' then 0 else
	if(c=='A') then 1 else
	if(c=='B') then 2 else
	if(c=='C') then 3 else
	if(c=='D') then 4 else
	if(c=='E') then 5 else
	if(c=='F') then 6 else
	if(c=='G') then 7 else
	if(c=='H') then 8 else
	if(c=='I') then 9 else
	if(c=='J') then 10 else
	if(c=='K') then 11 else
	if(c=='L') then 12 else
	if(c=='M') then 13 else
	if(c=='N') then 14 else
	if(c=='O') then 15 else
	if(c=='P') then 16 else
	if(c=='Q') then 17 else
	if(c=='R') then 18 else
	if(c=='S') then 19 else
	if(c=='T') then 20 else
	if(c=='U') then 21 else
	if(c=='V') then 22 else
	if(c=='W') then 23 else
	if(c=='X') then 24 else
	if(c=='Y') then 25 else
	if(c=='Z') then 26 else -1;;

(*
    @requires: prend un nombre de 0 à 26 ou -1.
    @ensures : cette fonction va prendre un entier et renvoie le caractère correspondant .
	          si l'etier entré est -1 la fonction retourne un 'Z' et si c'est autre entier différent de -1 et n'appartient pas à [0,26] la fonction
              retourne ' '.

*)

let index_char = fun c ->
	
	if (c == 0) then ' ' else
	if(c==1) then 'A' else
	if(c==2) then 'B' else
	if(c==3) then 'C' else
	if(c==4) then 'D' else
	if(c==5) then 'E' else
	if(c==6) then 'F' else
	if(c==7) then 'G' else
	if(c==8) then 'H' else
	if(c==9) then 'I' else
	if(c==10) then 'J' else
	if(c==11) then 'K' else
	if(c==12) then 'L' else
	if(c==13) then 'M' else
	if(c==14) then 'N' else
	if(c==15) then 'O' else
	if(c==16) then 'P' else
	if(c==17) then 'Q' else
	if(c==18) then 'R' else
	if(c==19) then 'S' else
	if(c==20) then 'T' else
	if(c==21) then 'U' else
	if(c==22) then 'V' else
	if(c==23) then 'W' else
	if(c==24) then 'X' else
	if(c==25) then 'Y' else
	if(c==26) then 'Z' else 
	if(c==(-1)) then 'Z' else ' ';;


(* LES DEUX FONCTION SUIVANTE DISTANCE_N ET DISTANCE_P SERONT UTILISÉ POUR ÉTABLIR LE CHOIX LE PLUS OPTIMISÉ ENTRE N ET P*)

(*
    @requires: les paramétres font partie de l'alphabet latin et en MAJISCULE
    @ensures: la fonction distance_N calcule la distance entre deux caractéres entrés dans le dans le sens des aiguilles d'une montre
            la fonction distance_P calcule la distance entre deux caractéres entrés dans le dans le sens inverse des aiguilles d'une montre			   
	      

*)

let rec distance_N a b = if(a!=b) then 1+distance_N  (index_char(char_index(a)+1)) b else 0;;
let rec distance_P a b = if(a!=b) then 1+distance_P  (index_char (char_index(a)-1)) b else 0;;

(*
   @requires:prend deux caractères  de l'alphabet latin et en MAJISCULE, le premier représente la position initial d'une roue et le deuxième le 
             caractère que nous voulons atteindre
   @ensures:cette fonction va renvoyer l'instruction pour passer d'une lettre à une autre par exemple (instruction_lettre ' ' 'A') doit retourner NE.
	

*)

let rec instruction_lettre a b = 
    if(a!=b) then 
	if((distance_N a b)<=(distance_P a b)) then 
		"N"^instruction_lettre (index_char(char_index(a)+1)) b
	else "P"^instruction_lettre (index_char(char_index(a)-1)) b
    else ""^"E";;



(** PHASE 1 : TRANSMISSION D'UN MESSAGE AVEC UNE ANTENNE **)

(*
   @requires:prend un caractère représentant la position initial d'une roue et une liste de caractère représentant un message
   @ensures:cette fonction va renvoyé la commande finale pour un message par exemple ( instruction_msg ' ' (split_string "AZ")) doit retourner NEPPE.
	     autrement dit elle fait un parcours du message représenté par une liste de charactères et applique la fonction instruction_lettre pour chaque
         lettre du message.Tous les caractères doivent être en MAJUSCULE.
	
*)

let rec instruction_msg pos msg =
	match msg with 
	[] -> ""
	|t::q -> instruction_lettre pos t ^ instruction_msg t q;;

(** PHASE 2 : FONCTIONS AUXILIÈRES UTILES POUR CETTE PHASE **)


(*
   @requires:les paramétres sont un entier qui représente le nombre d'antennes, le deuxième est la position initial de tous les antennes le troisième
              paramétre est le numéro de la première roue par lequel on veut commencé le numération des antennes  
  
   @ensures: cette fonction va construire une liste initiale de couples le premier élément de chaque couple représente le numéro de l'antenne qu'il
             représente,le deuxième est la position initiale de l'antenne
	         par exemple liste_roue 2 ' ' 0 doit nous retourner la liste [(0,' ');(1,' ')] (deux roues numérotés de 0 à 1).

*)

let rec liste_roue n pos i =
	
   if(i<n) then (i,pos)::liste_roue n pos (i+1) else [];;

(*

   @requires : une liste de couples (@see liste_roue ), la position du couple à remplacé, le couple remplaçant
   @ensures  : à chaque fois qu'une roue se déplace d'un caractère à un autre sa position représenté par le deuxième élément d'un couple doit être
               remplacé par sa nouvelle position, la fonction replace assure la fonctionnalité de remplacement.

*)


let replace l pos a  = List.mapi (fun i x -> if i = pos then a else x) l;;

(*
   @requires : prend en parametre une liste de couples et un caractère
   @ensures: cette fonction doit retourner une liste des distances entre la position de chaque roue et un caractère  dans le sens des aiguilles d'une
             montre 


*)

let rec grouper_distance_N l c = match l with
	[]-> []
	|(a,b)::q -> (distance_N b c) ::grouper_distance_N q c;;

(*
   @requires : prend en parametre une liste de couples et un caractère
   @ensures: cette fonction doit retourner une liste des distances entre la position de chaque roue et un caractère  dans le sens CONTRAIRE des
             aiguilles d'une montre 

*)

let rec grouper_distance_P l c = match l with
	[]-> []
	|(a,b)::q -> (distance_P b c) ::grouper_distance_P q c;;

(*
   @requires : une liste d'entiers qui représente des distances
   @ensures: retourne la distance minimal.


*)

let min_distance l = List.fold_left (fun a b ->min a b) max_int l;;

(*
   @requires : deux listes d'entiers qui représentent les distances en utilisant N et les distances en utilisant P
   @ensures: retourne le minmum des minimums de ces deux listes

*)

let min_N_P ln lp  = min (min_distance ln ) (min_distance lp );;

(*

  @requires : une liste des couples représentant les roues, un caractère.
  @ensures : cette fonction va prendre une liste des roues et en fonction du deuxième paramétre (le caractère qu'on veut atteindre ) elle doit nous
             renvoyé la bonne roue à utilisé(la plus proche).

*)

let rec choix_roue_lettre l c  = match l with 	
	       [] -> (0,' ')
	       |(a,b)::q -> let ln = grouper_distance_N l c in let lp=grouper_distance_P l c in  let min_d = min_N_P ln lp  in
			    if (min_d == (distance_N b c) || min_d == (distance_P b c) ) then (a,b)
			    else choix_roue_lettre q c;;
	      

(** PHASE 2 : UTILISATION DE PLUSIEURS ANTENNES **)

(*

  @requires : une liste des couples représentant les roues , une liste de caractères représentant un message un entier 
  @ensures : cette fonction retourne l'instruction pour un message construit par plusieurs roues par exemple
	      instruction_msg_nroue [(0,' ');(1,' ')] (split_string "AZ") doit retourner la chaine NEs1PE.

*)

let rec instruction_msg_nroue  l  msg i =
	match msg with 
	[] -> ""
       |t::q ->if (i == 0 ) then
		      let l'=replace l 0 (0,t)  in instruction_lettre ' ' t ^ instruction_msg_nroue l'  q (i+1)
	       else let roue_active = choix_roue_lettre l t  in let l'=replace l (fst(roue_active)) (fst(roue_active),t)  in
		    "s"^string_of_int(fst(roue_active))^instruction_lettre (snd(roue_active)) t ^ instruction_msg_nroue l'  q (i+1);;

(** CALCUL DU TEMPS **)

(*

  @requires : une chaine représentant la commande d'un message
  @ensures : Cette fonction retourne le temps demandé pour l'application de la commande passée en paramétres

*)


let rec time  inst = match inst with
	[] -> 0
       |t::q -> 
		if(t=='N' || t == 'P') then 3+ time q
		else if (t=='E') then 5+ time q 
		else if (t=='s') then 1+time q
		else 0+time q;; 

(*

  @requires : prend en parametre le nombre d'antenne et un message
  @ensures : en fonction de n applique la valeur retournée par instruction_msg ou instruction_msg_nroue à la fonction time
*)


let communication_time n msg = 
	if(n == 1) then let inst = instruction_msg ' '  msg in  time (split_string (inst))
	else let roues = liste_roue n ' ' 0 in let inst = instruction_msg_nroue roues msg 0 in  time (split_string (inst));;

(*

  @requires : prend en paramétre le nombre d'antennes et un message
  @ensures : cette fonction compare le temps nécessaire pour une commande en utilisant chacune des deux solutions et en fonction de cette comparaison
			 elle propose le bon choix.

*)

let compare_time n msg =
	if (n==1 && (communication_time 2 msg) < (communication_time n msg)) then  "time="^ string_of_int (communication_time n msg)
			^"s \nen utlisant 2 roues je vais passé "^string_of_int (communication_time 2 msg)^"s\nPensez à ajouter des roues c'est plus optimisé!"
	else if(n==1 && (communication_time 2 msg) > (communication_time n msg) ) then "time="^ string_of_int (communication_time n msg)^"s"

	else if(n!=1 && (communication_time n msg) < (communication_time 1 msg)) then "time ="^string_of_int(communication_time n msg) 
	else "time="^string_of_int (communication_time n msg)^"s"^"\navec une seule roue je vais passer "^string_of_int (communication_time 1 msg)^"s\npour ce message c'est mieux d'utiliser une seule roue";;


(*******TEST DES FONCTIONS ****)

(*
  @requires : le nombre de roues et la liste qui représente un message
  @ensures : si n ==1 on va utiliser une seule roue sinon n roues

*)


let communication n msg = 
	if(n == 1) then instruction_msg ' '  msg
	else let roues = liste_roue n ' ' 0 in instruction_msg_nroue roues msg 0;;



Printf.printf "Entrez le nombre de roues et votre message en MAJUSCULE\n"
let entre = parse_input();;
let msg = snd(entre);;
let n = fst(entre);;

(*Affichage de la commande, le temps demandé par cette commande et*)

Printf.printf "%s\n%s\n" (communication n msg)(compare_time n msg);;



