
SRC_DIR=./src/
SRC_MLI=$(wildcard $(SRC_DIR)/*.mli)
SRC_ML=$(wildcard $(SRC_DIR)/*.ml)

tools.cmo: graph.cmo tools.mli tools.ml
	ocamlc -c tools.mli tools.ml

gfile.cmo: gfile.mli gfile.ml
	ocamlc -c gfile.mli gfile.ml

graph.cmo: graph.mli graph.ml
	ocamlc -c graph.mli graph.ml

ftest: graph.cmo gfile.cmo tools.cmo ftest.ml
	ocamlc -o ftest graph.cmo gfile.cmo tools.cmo ftest.ml

all: graph.cmo gfile.cmo tools.cmo

.PHONY: clean
clean:
	rm -f ./*.cmi ./*.cmo ftest

