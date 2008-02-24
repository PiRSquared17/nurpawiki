#!/bin/sh

N=100
BN=500

test_wiki_start() 
{
    i=0

    while [ "$i" -lt $N ]; do
        curl "http://localhost:8080/view?p=WikiStart"  > /dev/null 2> /dev/null
        i=`expr $i + 1`
    done
}

test_history() 
{
    i=0

    while [ "$i" -lt $N ]; do
        curl "http://localhost:8080/history" > /dev/null 2> /dev/null
        i=`expr $i + 1`
    done
}

test_benchmark() 
{
    i=0

    while [ "$i" -lt $BN ]; do
        curl "http://localhost:8080/benchmark?test=$1" > /dev/null 2> /dev/null
        i=`expr $i + 1`
    done
}

echo View WikiStart $N times
time -p test_wiki_start
echo
echo "View /history $N times"
time -p test_history
echo
echo "View /benchmark?test=empty $BN times"
time -p test_benchmark empty
echo
echo "View /benchmark?test=db1 $BN times"
time -p test_benchmark db1
