all: evaluation expr miniml

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

miniml: miniml.ml
	ocamlbuild -use-ocamlfind miniml.byte
	
clean:
	rm -rf _build *.byte