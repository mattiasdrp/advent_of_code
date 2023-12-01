#!/bin/bash

function two_digits_zero_fill
{
    # print the number as a string with a leading zero
    printf -v name "day%02d" "$1"
    printf -v name_example "day%02d-example" "$1"
    touch $name
    touch $name_example
}

for i in {1..25}; do two_digits_zero_fill "$i"; done
