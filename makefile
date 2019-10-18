all: expr miniml tests

expr: expr.ml
	ocamlbuild expr.byte

tests: tests.ml
	ocamlbuild tests.byte

miniml: miniml.ml
	ocamlbuild miniml.byte

clean:
	rm -rf _build *.byte