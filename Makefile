

graphite: _build/graphite.cmx _build/graphite.cmo

_build/graphite.cmx: graphite.ml
	ocamlbuild -use-ocamlfind graphite.cmx

_build/graphite.cmxa: graphite.ml
	ocamlbuild -use-ocamlfind graphite.cmxa

_build/graphite.cmo: graphite.ml
	ocamlbuild -use-ocamlfind graphite.cmo

install: _build/graphite.cmo _build/graphite.cmx _build/graphite.cmxa
	ocamlfind install graphite META \
		_build/graphite.cm[iox] _build/graphite.cmxa _build/graphite.o # _build/graphite.mli

uninstall:
	ocamlfind remove graphite
