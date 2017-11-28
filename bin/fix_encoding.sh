#!/bin/bash

# Changes the encoding on HL CSV files from latin1 to UTF-8
iconv -f latin1 -t UTF-8 $1 > /var/tmp/tmp.csv

rm $1 
mv /var/tmp/tmp.csv $1
