module GraphPrinter (prettyPrint, prettyDetailedPrint, prettyDetailedPrintEs, prettyDetailedPrintNs) where

-- LIBRARIES
import qualified Data.Graph.Inductive as G (DynGraph, context, nodes, Context)
import qualified GraphMonad as GM (State, Node(Node), Edge(Edge), SFix)
import Data.Interval (Interval, Extended (..), lowerBound, upperBound)
import GraphMonad (Edge(esfix))


-- Pretty-print the graph details to stdout
prettyDetailedPrint :: GM.State -> IO ()
prettyDetailedPrint = putStr . prettifyDetailed True True

-- Pretty-print the edge details to stdout
prettyDetailedPrintEs :: GM.State -> IO ()
prettyDetailedPrintEs = putStr . prettifyDetailed False True

-- Pretty-print the node details to stdout
prettyDetailedPrintNs :: GM.State -> IO ()
prettyDetailedPrintNs = putStr . prettifyDetailed True False


-- Pretty-print the graph details (kept in the state)
prettifyDetailed :: Bool -> Bool -> GM.State -> String
prettifyDetailed showNs showEs (ns, es) = (if showNs then foldr showsNode id ns "" else "") ++ 
                                          (if showEs then foldr showsEdge id es "" else "")
        where
                showsNode :: GM.Node a -> (a2 -> String) -> a2 -> String
                showsNode (GM.Node i id t p r _) sg = shows i 
                                        . showString ": " . shows p
                                        . showString ", " . shows r
                                        . showString ", " . shows t
                                        . showString "\n\n" . sg
                showsEdge :: GM.Edge a -> (a2 -> String) -> a2 -> String
                showsEdge (GM.Edge i i1 i2 t a interval sfix) sg = shows i1 . showString " -> " . shows i2 
                                                . showString ": Argument " . shows a
                                                . showString (", " ++ prettifyInterval interval)
                                                . showString ", " . shows t
                                                . showString ", " . shows sfix
                                                . showString "\n\n" . sg        

-- Pretty-print the graph to stdout
prettyPrint :: (G.DynGraph gr, Show a, Show b) => gr a b -> IO ()
prettyPrint = putStr . prettify

-- Pretty-print the graph.  Note that this loses a lot of
-- information, such as edge inverses, etc.
prettify :: (G.DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = foldr (showsContext . G.context g) id (G.nodes g) ""
  where
    showsContext :: (Show a, Show a1) => G.Context a a1 -> (a2 -> String) -> a2 -> String
    showsContext (_,n,l,s) sg = shows n . showString ": " . shows l
                                . showString " -> " . shows s
                                . showString "\n\n" . sg

-- Pretty-print the interval
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
