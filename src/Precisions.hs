module Precisions (inputLSB, outputLSB, maxLSB, defaultMSB) where 

-- MSB --

defaultMSB :: Int
defaultMSB = 23

-- LSB --

inputLSB :: Int
inputLSB = -4

outputLSB :: Int
outputLSB = -2

-- The precision used by DAC (hi-res)
maxLSB :: Int 
maxLSB = -23
