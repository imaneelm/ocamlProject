# ocamlProject

Le but de ce projet est d’implanter un protocole de communication avec les habitants des mondes
lointains.
Plus précisément, il s’agira de prendre un message entrée et de donner la suite d’instructions permettant
de piloter une (ou plusieurs) antenne(s) afinn d’envoyer ledit message.
Dans l’urgence de cette nouvelle, nous avons développé un système d’antennes rudimentaire composées
chacune d’une roue comportant les 26 lettres de l’alphabet et du caractère espace. Nous pouvons
donc voir l’ensemble des symboles utilisables comme un cycle. Le caractère suivant A est B, ..., le
caractère suivant Y est Z, le caractère suivant Z est espace suivi de A.
Toutes les antennes sont initialement positionnées sur le caractère espace.
Les commandes disponibles pour manipuler une antenne sont au nombre de 3 :
"N" : passe au caractère suivant. ;
"P" : revient au caractère précédent ;
"E" : envoie la commande d’émission du caractère courrant.
