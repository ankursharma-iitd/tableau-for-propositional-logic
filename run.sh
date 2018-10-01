#!/bin/sh
ocamlc sig.mli
ocamlc arg.ml
ocamlc arg.cmo str.ml -o mytableau
./mytableau
dot -Tpng mygraph.dot -o mygraph.png