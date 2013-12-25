#!/bin/sh

wget -r -H -nc -np -nH --cut-dirs=2 -e robots=off -l1 -i ./market-list.txt -B 'http://api.bitcoincharts.com/v1/csv/'
