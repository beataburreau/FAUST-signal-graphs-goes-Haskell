{-# LANGUAGE RankNTypes #-}

module GraphState
    (GraphState,
    Info,
    Edge(Edge),
    Node(Node, nid, ni),
    Primitive(..),
    ComputationRate(..),
    Type(..),
    toPrimitive,
    toComputationRate,
    toType,
    toInterval)
where

-- MONADS
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import Control.Exception (Exception)

-- LIBRARIES
import Data.Interval (Interval, Boundary (..), interval, Extended (NegInf, PosInf, Finite))
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Text.Parsec (Parsec, parse, oneOf, manyTill, string, digit, spaces, many1, many, option, choice, char)
import Numeric (readHex)
import Data.Char (toLower)


-- The graph state 
-- Keeps information about the graph's nodes and edges while allowing IO action
type GraphState a = StateT Info IO a

-- The information tuple
-- Holds node information in the first tuple element and edge information in the second
type Info = ([Node Int], [Edge Int])

-- Node data type
data Node ni = Node {
    ni :: Int,      -- unique integer indentifier
    nid :: String,  -- identifier in .dot graph, e.g. S0x7fb9cfd0abe0
    ntype :: Type,
    nprimitive :: Primitive,
    nrate :: ComputationRate
}

-- Edge data type
data Edge ei = Edge {
    ei :: Int,      -- unique integer indentifier 
    ei1 :: Int,     -- start node identifier 
    ei2 :: Int,     -- end node identifier
    etype :: Type,
    einterval :: Interval Float
}

data Type = Integer
          | Float

data Primitive = Number (Either Int Float) -- a number is _either_ an integer or a floating point number
               | Operator String
               | UI String
               | Output

data ComputationRate = Constant
                     | Sample
                     | Block

-- Converts a .dot graph color into the corresponding type
toType :: String -> Type
toType str = case str of
                "blue"  -> Integer
                "blue2" -> Integer
                "red"   -> Float
                "red2"  -> Float
                _      -> error "Not a valid type"

-- Converts a .dot graph node label into the corresponding primitive
toPrimitive :: String -> Primitive
toPrimitive str | isJust number         = Number $ fromJust number
                | str `elem` operators  = Operator str
                | str `elem` uielements = UI str
                | str == "OUTPUT_0"     = Output
                | otherwise             = error $ str ++ " is not a valid primitive value; it must be an integer, float, operator, UI element or output signal"
                where
                    number = toNumber str
                    operators = ["int", "float", "!", "@", "^", "*", "/", "%", "+", "-", "<", "<=", ">", ">=", "==", "!=", "xor", "&", "|", "<<", ">>"] -- currently non-exhaustive
                    uielements = ["button", "checkbox", "hslider", "vslider", "nentry"]
                    toNumber :: String -> Maybe (Either Int Float)
                    toNumber str = case readMaybe str :: Maybe Int of
                                    Just i  -> Just $ Left i
                                    Nothing -> case readMaybe str :: Maybe Float of
                                        Just d -> Just $ Right d
                                        Nothing -> Nothing

-- Converts a .dot graph node shape into the corresponding computation rate
toComputationRate :: String -> ComputationRate
toComputationRate str = case str of
                            "square"  -> Constant
                            "ellipse" -> Sample
                            "box"     -> Block
                            _         -> error "Not a valid computation rate"

-- Parses a .dot graph edge label into a floating point interval using Parsec parsers
-- EX/ 
-- "[4.000000, inf], r(???)" is successfully parsed as (Finite 4.00 <=..< PosInf)
toInterval :: String -> Interval Float
toInterval str = interval (extendedF lower) (extendedF upper)
                where
                    (lower, upper) = case parse intervalP "" str of
                                    Left _ -> error $ str ++ " is not a valid interval"
                                    Right (fst, snd) -> (fst, snd)
                    extendedF :: String -> (Extended Float, Boundary)
                    extendedF str = case str of 
                                        "inf"   -> (PosInf, Open)
                                        "-inf"  -> (NegInf, Open)
                                        _       -> (Finite $ toFloat str, Closed)
                    toFloat :: String -> Float
                    toFloat str = case readMaybe str :: Maybe Float of
                                    Just d -> d
                                    Nothing -> error $ "Failed to parse " ++ str ++ " as floating point number"
                    -- Parsec parser for intervals
                    intervalP :: Parsec String () (String, String)
                    intervalP = do
                                    char '['
                                    fst <- choice [floatP, infinityP]
                                    char ','
                                    spaces
                                    snd <- choice [floatP, infinityP]
                                    char ']'
                                    many (oneOf ", r(?")
                                    return (fst, snd)
                    -- Parsec parser for floating point numbers
                    floatP :: Parsec String () String
                    floatP = do
                                sign <- option "" (string "-")
                                intPart <- manyTill digit (char '.')
                                decPart <- many1 digit
                                return $ sign ++ intPart ++ "." ++ decPart
                    -- Parsec parser for (positive and negative) infinities
                    infinityP :: Parsec String () String
                    infinityP = do 
                                sign <- option "" (string "-")
                                string "inf"
                                return $ sign ++ "inf"
