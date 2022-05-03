module GraphPrinter (prettyPrint) where 

-- LIBRARIES
import Data.Graph.Inductive (DynGraph, context, nodes)

-- Pretty-print the graph to stdout.
prettyPrint :: (DynGraph gr, Show a, Show b) => gr a b -> IO ()
prettyPrint = putStr . prettify

-- | Pretty-print the graph.  Note that this loses a lot of
--   information, such as edge inverses, etc.
prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = foldr (showsContext . context g) id (nodes g) ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows l
                                . showString " -> " . shows s
                                . showString "\n\n" . sg