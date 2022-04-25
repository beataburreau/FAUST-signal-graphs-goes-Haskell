{-# LANGUAGE RankNTypes #-}

module GraphState
    (GraphState,
    Info,
    Edge(Edge),
    Node(Node, nid, ni),
    Primitive(..),
    ComputationRate(..),
    Type(..),
    toIndex,
    toPrimitive,
    toComputationRate,
    toType,
    toInterval, 
    getLowerBound, 
    getUpperBound)
where

-- MONADS
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import Control.Exception (Exception)

-- LIBRARIES
import Data.Interval (Interval, (<=..<=), lowerBound, Extended (NegInf, PosInf, Finite), upperBound)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Text.Parsec (Parsec, parse, oneOf, manyTill, digit, spaces, many1, many)
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

-- Produces a integer index given a .dot graph identifier, e.g. S0x7fb9cfd0abe0
-- The hexadecimal part of the identifier is converted to an integer with readHex
toIndex :: String -> Int
toIndex id = case readHex $ drop 3 id of 
                []  -> error "Failed conversion from id to integer index"
                res -> fst $ head res

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
-- "[4.000000, 4.000000], r(???)" is successfully parsed as (Finite 4.00 <=..<= Finite 4.00)
toInterval :: String -> Interval Float
toInterval str = Finite lower <=..<= Finite upper
                where
                    (lower, upper) = case parse interval "" str of
                                    Left _ -> error $ str ++ " is not a valid interval"
                                    Right (fst, snd) -> (toFloat fst, toFloat snd)
                    toFloat :: String -> Float
                    toFloat str = case readMaybe str :: Maybe Float of
                                    Just d -> d
                                    Nothing -> error $ "Failed to parse " ++ str ++ " as floating point number"
                    -- Parsec parser for intervals
                    interval :: Parsec String () (String, String)
                    interval = do
                                    oneOf "["
                                    fst <- float
                                    oneOf ","
                                    spaces
                                    snd <- float
                                    oneOf "]"
                                    many (oneOf ", r(?")
                                    return (fst, snd)
                    -- Parsec parser for floating point numbers
                    float :: Parsec String () String
                    float = do
                            intPart <- manyTill digit (oneOf ".")
                            decPart <- many1 digit
                            return $ intPart ++ "." ++ decPart

-- Getter for lower bound of a given interval
-- Returns 'raw' floating point number
getLowerBound :: Interval Float -> Float 
getLowerBound = getBound "lower"

-- Getter for upper bound of a given interval
-- Returns 'raw' floating point number
getUpperBound :: Interval Float -> Float 
getUpperBound = getBound "upper"

-- Getter for given bound ("lower" or "upper") of given interval
-- Returns the 'raw' floating point number (i.e. sans Finitie constructor) if 
-- the bound is a finite value
-- Throws an error if the bound is the positive or negative infinity
getBound :: String -> Interval Float -> Float
getBound boundType interval = case bound of 
                                Finite f -> f
                                NegInf     -> error $ "The " ++ boundType ++ " bound is negative infinity (-∞)"
                                PosInf     -> error $ "The " ++ boundType ++ " bound is positive infinity (∞)"
                             where bound = case map toLower boundType of 
                                            "lower" -> lowerBound interval
                                            "upper" -> upperBound interval
                                            _       -> error $ boundType ++ " is not a valid bound type"


