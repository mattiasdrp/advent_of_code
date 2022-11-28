#!/bin/bash

for dir in $1/*/ ;
do
    echo "----$dir----"
    cd $dir
    res=$(dune clean)
    cd -
done
