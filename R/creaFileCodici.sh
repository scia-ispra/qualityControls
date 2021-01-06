#!/bin/bash

head -n1 precipitazione.csv | sed -e 's/;/\n/g' >temp.txt


head -n1 tmax.csv | sed -e 's/;/\n/g' >>temp.txt


head -n1 tmin.csv | sed -e 's/;/\n/g' >>temp.txt

cat temp.txt | sort | uniq  > temp2.txt

grep -v -E "^[ymd].+"  temp2.txt > codici.txt

rm -rf temp.txt
rm -rf temp2.txt
