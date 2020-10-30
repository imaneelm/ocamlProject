
(*	    
  Donne une commande pour un message avec une seule roue représenté par une liste des caractères 
  exemple ( instruction_msg ' ' (split_string "IM")) doit retourner NEPPE.
  Tous les caractères doivent être en MAJUSCULE.	
*)


val instruction_msg : char -> char list -> string 


(*

  Donne la commande de transmission  pour un message avec n roues 
  exemple ( instruction_msg_nroue [(0,' ');(1,' ')] (split_string "AZ")) doit retourner NES1PE.
  Tous les caractères doivent être en MAJUSCULE.	

*)

val instruction_msg_nroue : (int * char) list -> char list -> int-> string









