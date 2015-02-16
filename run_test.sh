#!/bin/bash

count=$(wc -l $1 | cut -f 1 -d ' ' )
eval_count=$(expr $count / 10)
train_count=$(expr $count - $eval_count)

echo there are $count lines, $eval_count will go to eval, $train_count for training
sort -R $1 > tmp_rand
head -n $eval_count $1 > eval.data
tail -n $train_count $1 > training.data
rm tmp_rand

./a.out
dot -Tsvg tree.dot > render.svg
