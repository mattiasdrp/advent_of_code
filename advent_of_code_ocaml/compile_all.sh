#!/bin/bash

for dir in $1/*/ ;
do
    echo "----$dir----"
    cd $dir
    start=`date +%s.%N`
    res=$(dune exec program)
    end=`date +%s.%N`
    runtime=$( echo "$end - $start" | bc -l )
    nb=$(echo "$dir" | sed -n "s/^[0-9]*\/day\([0-9a-z]*\).*/\1/p")
    exp=$(grep -hr "^$nb\." ../solutions | sed -n "s/[0-9a-z]*\. \(.*\)/\1/p")
    RED='\033[31m'
    GREEN='\033[32m'
    GRAY='\033[90m'
    BOLD='\033[1;90m'
    NC='\033[0m' # No Color
    if [[ "$res" = "$exp" ]]; then
       printf "${GREEN}Passed${NC}\n"
       printf "${GREEN}  Result: $res${NC}\n"
    else
        printf "${RED}  Failed: Expected\n  '$exp'\ngot\n  '$res'${NC}\n"
        exit 1
    fi
    printf "  ${BOLD}Time: ${runtime}ms${NC}\n\n"
    cd -
done
