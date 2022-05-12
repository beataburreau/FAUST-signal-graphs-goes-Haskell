{-# LANGUAGE LambdaCase #-}
module GraphPrinter (prettyPrint, prettyDetailedPrint) where

-- LIBRARIES
import Data.Graph.Inductive (DynGraph, context, nodes, Context)
import GraphMonad (State, Node(Node), Edge(Edge))
import Data.Interval (Interval, Extended (..), lowerBound, upperBound)


-- Pretty-print the graph details to stdout.
prettyDetailedPrint :: State -> IO ()
prettyDetailedPrint = putStr . prettifyDetailed

-- Pretty-print the graph details (kept in the state)
prettifyDetailed :: State -> String
prettifyDetailed (ns, es) = foldr showsNode id ns "" ++ foldr showsEdge id es ""
        where
                showsNode :: Node a -> (a2 -> String) -> a2 -> String
                showsNode (Node i id t p r) sg = shows i 
                                        . showString ": " . shows p
                                        . showString ", " . shows r
                                        . showString ", " . shows t
                                        . showString "\n\n" . sg
                showsEdge :: Edge a -> (a2 -> String) -> a2 -> String
                showsEdge (Edge i i1 i2 t a interval) sg = shows i1 . showString " -> " . shows i2 
                                                . showString ": Argument " . shows a
                                                . showString (", " ++ prettifyInterval interval)
                                                . showString ", " . shows t
                                                . showString "\n\n" . sg        

-- Pretty-print the graph to stdout.
prettyPrint :: (DynGraph gr, Show a, Show b) => gr a b -> IO ()
prettyPrint = putStr . prettify

-- Pretty-print the graph.  Note that this loses a lot of
-- information, such as edge inverses, etc.
prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = foldr (showsContext . context g) id (nodes g) ""
  where
    showsContext :: (Show a, Show a1) => Context a a1 -> (a2 -> String) -> a2 -> String
    showsContext (_,n,l,s) sg = shows n . showString ": " . shows l
                                . showString " -> " . shows s
                                . showString "\n\n" . sg


prettifyInterval :: Maybe (Interval Float) -> String
prettifyInterval Nothing   = "[?, ?]"
prettifyInterval (Just i)  = lower ++ ", " ++ upper
                    where
                        lower = case lowerBound i of
                                PosInf      -> "(inf"
                                NegInf      -> "(-inf"
                                (Finite f)  -> "[" ++ show f
                        upper = case upperBound i of
                                PosInf      -> "inf)"
                                NegInf      -> "-inf)"
                                (Finite f)  -> show f ++ "]"