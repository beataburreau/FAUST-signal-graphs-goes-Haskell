#!/bin/sh

for f in test-files/*.dsp 
do
    faust -sg $f
done


