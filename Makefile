all:
	ocamlc -c projet.mli
	ocamlc -c projet.ml
	ocamlc -o projet projet.cmo
	./projet


 
