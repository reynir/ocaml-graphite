

graphite: _build/graphite.cmx _build/graphite.cmo

_build/graphite.cmx: graphite.ml
	ocamlbuild -use-ocamlfind graphite.cmx

_build/graphite.cmo: graphite.ml
	ocamlbuild -use-ocamlfind graphite.cmo

install: _build/graphite.cmo _build/graphite.cmx
	ocamlfind install graphite META _build/graphite.cm[iox] # _build/graphite.mli

uninstall:
	ocamlfind remove graphite
