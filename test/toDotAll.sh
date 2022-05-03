#!/bin/sh

for f in test-files/*.dsp 
do
    faust --signal-graph $f
done


