#!/bin/sh

mkdir tmp
for i in `seq 0 10000`; do
    echo $i
    wget -r http://localhost:8080/view?p=WikiStart -o /dev/null -l1 -P tmp
done
