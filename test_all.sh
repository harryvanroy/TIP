#!/usr/bin/env bash
for T in $(cd tests; ls *.tip)
do
    echo $T
    ./tip -normalizereturns -interval wlrw tests/$T >out/$T.out
done
