module Main where

import SignalGraph (haskelliseFile)
import Data.List (isSuffixOf)
import qualified GraphMonad as GM (runGraphM, initState, State)
import LSBInference (inferLSBs, trimLSBs)
import GraphPrinter (prettyDetailedPrint, prettyDetailedPrintEs)

-- Parses the content of given file into an inductive dynamic graph and prints it
main :: IO ()
main = do
        putStrLn "Enter relative file path to a .dot file:"
        path <- getLine
        if ".dot" `isSuffixOf` path
        then do
                res <- GM.runGraphM GM.initState (haskelliseFile path)
                case res of
                        Left err          -> putStrLn $ "\nParse failed with error:\n" ++ show err
                        Right (gr, state) -> do putStrLn "Parse successful!\n"
                                                res <- GM.runGraphM state (inferLSBs gr 1)
                                                case res of 
                                                        Left err          -> putStrLn $ "\nLSB inference failed with error:\n" ++ show err
                                                        Right (gr, state) -> do putStrLn "LSB inference successful!\n" 
                                                                                prettyDetailedPrint state
                                                                                res <- GM.runGraphM state (trimLSBs gr 0) -- initializing alg. with node 0, the output node
                                                                                case res of
                                                                                        Left err          -> putStrLn $ "\nLSB trimming failed with error:\n" ++ show err
                                                                                        Right (gr, state) -> putStrLn "LSB trimming successful!\n" 
                                                                                                             >> prettyDetailedPrintEs state
                                                                             
        else errorWithoutStackTrace "The provided file is not a .dot file"
