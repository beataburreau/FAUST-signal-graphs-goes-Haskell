module Main where

import SignalGraph (haskelliseFile)
import Data.List (isSuffixOf)
import qualified GraphMonad as GM (runGraphM, initState, State, lookupEdge, Error (GraphError), cleanShow, Edge (esfix, ei1, ei2), SFix (lsb))
import LSBInference (inferLSBs)
import LSBTrimming (trimLSBs)
import GraphPrinter (prettyDetailedPrint, prettyDetailedPrintEs)
import Data.Graph.Inductive (size)

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
                                                res <- GM.runGraphM state (inferLSBs gr 0) -- initializing alg. with node 0, the output node
                                                case res of
                                                        Left err          -> putStrLn $ "\nLSB inference failed with error:\n" ++ show err
                                                        Right (gr, state) -> do putStrLn "LSB inference successful!\n"
                                                                                res <- GM.runGraphM state (trimLSBs gr 0) -- initializing alg. with node 0, the output node
                                                                                if size gr < 20
                                                                                then do 
                                                                                        prettyDetailedPrint state
                                                                                        case res of
                                                                                                Left err            -> putStrLn $ "\nLSB trimming failed with error:\n" ++ show err
                                                                                                Right (gr, state) -> do putStrLn "LSB trimming successful!\n"
                                                                                                                        prettyDetailedPrintEs state
                                                                                else do 
                                                                                        case res of
                                                                                                Left err            -> putStrLn $ "\nLSB trimming failed with error:\n" ++ show err
                                                                                                Right (gr, (_, es)) -> do putStrLn "LSB trimming successful!\n"
                                                                                                                          putStrLn "Edges:" >> printEs es >> printSfixREPL es

        else errorWithoutStackTrace "The provided file is not a .dot file"

printSfixREPL :: [GM.Edge Int] -> IO()
printSfixREPL es = do
        putStrLn "Which edge are you looking for the LSB of? Enter two nodes"
        putStrLn "Type :q to quit"
        n1 <- getLine
        if n1 == ":q" || n1 == ":quit"
        then return()
        else do
                n2 <- getLine
                case GM.lookupEdge (read n1) (read n2) es of
                        Nothing -> putStrLn ("No edge between " ++ show n1 ++ " and " ++ show n2 ++ " in graph, try again!") >> printSfixREPL es
                        Just e  -> print (GM.cleanShow e ++ ": " ++ show (GM.esfix e)) >> printSfixREPL es


printEs :: [GM.Edge Int] -> IO()
printEs = foldr ((>>) . putStrLn . GM.cleanShow) (putStrLn "")