#!/bin/bash/

for ((j=10;j<=90;j=j+10));
do

for ((i=1;i<=100;i++)); 
do

tail -n 5000 dat-$i-$j.out | awk '{sum1+=$3} {sum2+=$4} END {print sum1/5000,sum2/5000}'
done > temp


meannum=$(awk '{sum1+=$1} END {print sum1/NR}' temp)
meanlen=$(awk '{sum1+=$2} END {print sum1/NR}' temp)

errnum=$(awk -v meannum=$meannum '{sum+=($1-meannum)**2} END {print sqrt(sum/(NR-1))}' temp)
errlen=$(awk -v meanlen=$meanlen '{sum+=($2-meanlen)**2} END {print sqrt(sum/(NR-1))}' temp)

echo $j $meannum $meanlen $errnum $errlen

done

rm temp
