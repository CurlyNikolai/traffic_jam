#!/bin/bash

for ((j=10;j<=90;j=j+10));
do

  for ((i=1;i<=100;i++));
  do

    ./jam $i $j 100 10000 1 0 0
    mv dat.out dat-$i-$j.out

  done
done
