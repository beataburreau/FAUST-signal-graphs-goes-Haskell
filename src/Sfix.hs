module Sfix (inputLSB, outputLSB, maxLSB, recursionLSB, defaultMSB, recursionMSB) where 

-- MSB --

defaultMSB :: Int
defaultMSB = 23

recursionMSB :: Int
recursionMSB = 12

-- LSB --

inputLSB :: Int
inputLSB = -4

outputLSB :: Int
outputLSB = -2

recursionLSB :: Int
recursionLSB = -12

-- The precision used by DAC (hi-res)
maxLSB :: Int 
maxLSB = -23
