#!/bin/sh

for f in test-files/*.dsp 
do
    faust -svg $f
    NAME=$(echo "$f" | cut -f 1 -d '.')
    mv $NAME-svg/process.svg $NAME.svg
    rmdir $NAME-svg
done