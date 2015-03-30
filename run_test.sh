#!/bin/bash

sort -R $1 > training.data.full
split -a 1 -n l/5 -d training.data.full training.data
mv training.data4 eval.data

count=$(wc -l eval.data | cut -f 1 -d ' ' )
echo each training file has $count instances

# ocamldebug `ocamlfind query -recursive -i-format batteries` -I _build/src/ ./main.d.byte
./main.native
dot -Tsvg tree.dot > render.svg

rm training.data*
