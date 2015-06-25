#!/bin/bash

sort -R $1 > training.data.full
split -a 1 -n l/5 -d training.data.full training.data
mv training.data4 eval.data
cat training.data* > training.data

eval_count=$(wc -l eval.data | cut -f 1 -d ' ' )
train_count=$(wc -l training.data | cut -f 1 -d ' ' )
echo Using $train_count instances for training, $eval_count for evaluation

# ocamldebug `ocamlfind query -recursive -i-format batteries` -I _build/src/ ./main.d.byte
./main.p.native
# ./main.native
dot -Tsvg tree.dot > render.svg

rm training.data*
