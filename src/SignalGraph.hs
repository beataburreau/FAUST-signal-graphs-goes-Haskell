
module SignalGraph (haskelliseFile) where

-- PARSING
import SigDot.Abs (DotGraph)
import qualified SigDot.Abs as A
import SigDot.Par (pDotGraph, myLexer)

-- GRAPH STATE
import GraphMonad (GraphM, runGraphM, State, initState, Error(..), Edge(Edge),
      Node(Node, nid, ni), Primitive(..), ComputationRate(..), Type(..),
      toType, toPrimitive, toComputationRate, toInterval, Error (ParseError))

-- GRAPH PRINTER 
import GraphPrinter (prettyPrint)

-- MONADS
import Control.Monad.State (get, put, MonadIO (liftIO))
import Control.Exception (throw)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

-- LIBRARIES
import Data.Graph.Inductive (Gr, LNode, LEdge, Graph (mkGraph))
import Data.Char (isDigit)
import Data.List (stripPrefix)
import Data.Interval (lowerBound, upperBound, Extended (..))

-- Type synonyms for node and edge labels
-- (as used in the inductive dynamic graph type, Gr N E)
type N = String   -- Node label
type E = String   -- Edge label

-- Parses the content of given file into an inductive dynamic graph (Gr N E)
-- *  The nodes and edges in the graph are labelled with their primitives and intervals respectively
-- *  Additional graph information - such as types and computation rates - is kept in the graph monad, GraphM
haskelliseFile :: FilePath -> GraphM (Gr N E)
haskelliseFile path = liftIO (parseFile path) >>= makeGraph


-- Parses the content of given file into the abstract syntax tree structure, DotGraph 
parseFile :: FilePath -> IO DotGraph
parseFile path = parse <$> readFile path

-- Parses the given string into the abstract syntax tree structure, DotGraph 
parse :: String -> DotGraph
parse s = case pDotGraph (myLexer s) of
    Left s -> throw $ ParseError s
    Right p -> p

-- Creates an inductive dynamic graph (Gr N E) with labelled nodes and edges from the given AST structure (DotGraph)
-- *  Additional graph information is stored in the graph monad, GraphM
makeGraph :: DotGraph -> GraphM (Gr N E)
makeGraph (A.GDef _ _ stmts) = do
                                lnodes <- nodes stmts
                                ledges <- edges stmts
                                return $ mkGraph lnodes ledges


-- Constructs a list of labelled nodes from the given statement list
-- *  Additional node information is stored in the graph monad, GraphM; the information - id, type, primitive and computation rate - 
--    is retrieved from a node statement's attribute list as strings and converted into respective data type
nodes :: [A.Stmt] -> GraphM [LNode N]
nodes stmts = do
                (ns, es) <- get
                let (ns', lns) = unzip $ getNodes stmts 1
                put (ns', es)
                return lns
                where
                    getNodes :: [A.Stmt] -> Int -> [(Node Int, LNode N)]
                    getNodes []        count = []
                    getNodes (s:stmts) count = case node s count of
                                                Just n -> n:getNodes stmts (succ count)
                                                Nothing -> getNodes stmts count
                    node :: A.Stmt -> Int -> Maybe (Node Int, LNode N)
                    node s i = case s of
                                (A.SNode (A.ID id) attrs) -> if id /= "OUTPUT_0"
                                                             then Just (Node i id t p r, (i, l))
                                                             else Just (Node 0 id t p r, (0, id))
                                                                where l = case getAttribute attrs A.ALabel of
                                                                            Just l -> l
                                                                            Nothing -> throw $ ParseError "Missing label attribute"
                                                                      t = case getAttribute attrs A.AColor of
                                                                            Just c -> toType c
                                                                            Nothing -> throw $ ParseError "Missing color attribute"
                                                                      p = toPrimitive l
                                                                      r = case getAttribute attrs A.AShape of
                                                                            Just s -> toComputationRate s
                                                                            Nothing -> throw $ ParseError "Missing shape attribute"
                                _                         -> Nothing


-- Constructs a list of labelled edges from the given statement list
-- *  Additional edge information is stored in the graph monad, GraphM; the information - type and interval - 
--    is retrieved from a node statement's attribute list as strings and converted into respective data type
-- *  An edge is defined by the nodes it spans between, why an error is thrown if any of these nodes are missing from 
--    graph monad's node list
edges :: [A.Stmt] -> GraphM [LEdge E]
edges stmts = do
                (ns, es) <- get
                let (es', les) = unzip $ mapMaybe (edge ns) stmts
                put (ns, es')
                return les
                where
                    edge :: [Node Int] -> A.Stmt -> Maybe (Edge Int, LEdge N)
                    edge ns s = case s of
                                    (A.SEdge (A.ID id1) (A.ID id2) attrs) -> Just (Edge i i1 i2 t interval, (i1, i2, l))
                                                                                where i = i1 - i2
                                                                                      (i1, i2) = case lookupNodes (id1, id2) ns of
                                                                                                    Just is -> is
                                                                                                    _       -> throw $ ParseError "Couldn't find current nodes in the graph state's node list"
                                                                                      interval = case getAttribute attrs A.ALabel of
                                                                                            Just l -> toInterval l
                                                                                            Nothing -> throw $ ParseError "Missing label attribute"
                                                                                      l = case interval of
                                                                                            Just intrvl -> "[" ++ showExtendedF (lowerBound intrvl) ++ ", " ++ showExtendedF (upperBound intrvl) ++ "]"
                                                                                            Nothing     -> "[???]"
                                                                                      t = case getAttribute attrs A.AColor of
                                                                                            Just c -> toType c
                                                                                            Nothing -> throw $ ParseError "Missing color attribute"
                                    _                                     -> Nothing

-- Finds the integer identifiers for the nodes with the given ids, if both nodes are present in the list
lookupNodes :: (String, String) -> [Node Int] -> Maybe (Int, Int)
lookupNodes (id1, id2) ns = case lookupNode id1 ns of
                                Just i1 -> case lookupNode id2 ns of
                                    Just i2 -> Just (i1, i2)
                                    Nothing -> Nothing
                                Nothing -> Nothing

-- Finds the integer identifier for the node with the given id, if the node is present in the list
lookupNode :: String -> [Node Int] -> Maybe Int
lookupNode _  []     = Nothing
lookupNode id (n:ns) | nid n == id = Just $ ni n
                     | otherwise   = lookupNode id ns

-- Getter for string value of given attribute, if the attribute is present in the list of attributes
getAttribute :: [A.Attr] -> A.AKind -> Maybe String
getAttribute []                       _  = Nothing
getAttribute ((A.ARegular k1 str):as) k2 | k1 == k2  = Just str
                                         | otherwise = getAttribute as k2
getAttribute (a:as)                   k  = getAttribute as k

-- Alternative show function for Extended Float
-- Shows the 'raw' floating point number (i.e. sans Finitie constructor) in case of a finite value, 
-- and 'inf' / '-inf' in case of an infinite one 
showExtendedF :: Extended Float -> String
showExtendedF PosInf     = "inf"
showExtendedF NegInf     = "-inf"
showExtendedF (Finite f) = show f