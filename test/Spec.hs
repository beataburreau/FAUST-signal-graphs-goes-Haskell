module Main where 

import ParserTests (runTests)
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = sequence tests >>= \b -> if and b then exitSuccess else exitFailure
    where tests = [ ParserTests.runTests ]
