.PHONY: default clean

default: yaml_main.native

yaml_parser.mli: yaml_parser.mly
	corebuild -use-menhir $@

yaml_main.native: yaml_main.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core $@

yaml_only_lex.native: yaml_only_lex.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core $@

clean:
	-rm yaml_only_lex.native
	-rm yaml_main.native
	-rm -rf _build
