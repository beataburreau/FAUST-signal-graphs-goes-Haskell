#!/bin/sh

# Given name of a file in test-files
if [[ $1 == *.dsp ]]
then faust --signal-graph "test-files/${1}"
else echo "Not a .dsp file, could not generate .dot file" 
fi
