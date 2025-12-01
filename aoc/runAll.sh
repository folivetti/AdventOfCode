#!/bin/bash
#.stack-work/install/x86_64-linux/c63d0133edbd398a4307bc518bde7b59f82bff06d3c28600a234dc9d97aae851/9.4.6/bin/$1 +RTS -N8
#dist-newstyle/build/x86_64-linux/ghc-9.10.6/aoc-0.1.0.0/x/$1/build/$1/$1 +RTS -N8
for i in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 ; do
    echo $i
dist-newstyle/build/x86_64-linux/ghc-9.10.1/aoc-0.1.0.0/x/y${1}_${i}/build/y${1}_${i}/y${1}_${i} +RTS -N2
done
