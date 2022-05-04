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
import Data.Either (fromLeft)


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
    einterval :: Maybe (Interval Float)
}

data Type = Integer
          | Float

data Primitive = Number (Either Int Float) -- a number is _either_ an integer or a floating point number
               | Operator Operator
               | UI UI
               | Input Int
               | Output

data Operator = IntCast | FloatCast | Cut | Delay | Pow | Mul | Div | Mod | Add | Sub | LT | LE | GT | GE | EQ | NEQ | XOR | AND | OR | LShift | RShift

data UI = Button | Checkbox | HSlider | VSlider | Nentry

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
toPrimitive str | isJust number                           = Number $ fromJust number
                | isJust operator                         = Operator $ fromJust operator
                | isJust ui                               = UI $ fromJust ui
                | take 5 str == "INPUT" && isJust _number = Input $ fromLeft (error "Invalid input number, it must be an integer value") (fromJust _number)
                | str == "OUTPUT_0"                       = Output
                | otherwise                               = error $ str ++ " is not a valid primitive value; it must be an integer, float, operator, UI element, input or output signal"
                where
                    number = toNumber str
                    operator = toOperator str
                    ui = toUI str
                    _number = toNumber $ drop 6 str
                    toNumber :: String -> Maybe (Either Int Float)
                    toNumber str = case readMaybe str :: Maybe Int of
                                    Just i  -> Just $ Left i
                                    Nothing -> case readMaybe str :: Maybe Float of
                                        Just d -> Just $ Right d
                                        Nothing -> Nothing
                    toOperator :: String -> Maybe Operator
                    toOperator str = case str of
                                     "int"   -> Just IntCast
                                     "float" -> Just FloatCast
                                     "!"     -> Just Cut
                                     "@"     -> Just Delay
                                     "^"     -> Just Pow
                                     "*"     -> Just Mul
                                     "/"     -> Just Div
                                     "%"     -> Just Mod
                                     "+"     -> Just Add
                                     "-"     -> Just Sub
                                     "<"     -> Just GraphState.LT
                                     "<="    -> Just LE
                                     ">"     -> Just GraphState.GT
                                     ">="    -> Just GE
                                     "=="    -> Just GraphState.EQ
                                     "!="    -> Just NEQ
                                     "xor"   -> Just XOR
                                     "&"     -> Just AND
                                     "|"     -> Just OR
                                     "<<"    -> Just LShift
                                     ">>"    -> Just RShift
                                     _       -> Nothing
                    toUI :: String -> Maybe UI
                    toUI str = case str of
                                "button"   -> Just Button
                                "checkbox" -> Just Checkbox
                                "hslider"  -> Just HSlider
                                "vslider"  -> Just VSlider
                                "nentry"   -> Just Nentry
                                _          -> Nothing


-- Converts a .dot graph node shape into the corresponding computation rate
toComputationRate :: String -> ComputationRate
toComputationRate str = case str of
                            "square"  -> Constant
                            "ellipse" -> Sample
                            "box"     -> Block
                            _         -> error "Not a valid computation rate"

-- Parses a .dot graph edge label into a floating point interval using Parsec parsers
-- EX/ 
-- "[4.000000, inf], r(???)" is successfully parsed as a defined interval, giving Just (Finite 4.00 <=..< PosInf)
-- "[???], r(???)" is successfully parsed as an undefined interval,        giving Nothing
toInterval :: String -> Maybe (Interval Float)
toInterval str = case parse undefIntervalP "" str of 
                    Right _ -> Nothing
                    Left _  -> case parse defIntervalP "" str of 
                                Right (lower, upper) -> Just $ interval (bound lower) (bound upper)
                                Left _               -> error $ str ++ " is not a valid interval"
                where
                    -- Parsec parser for undefined intervals (on the form [???])
                    undefIntervalP :: Parsec String () String
                    undefIntervalP = do 
                                     string "[???]"
                                     many (oneOf ", r(?")
                                     return ""
                    -- Parsec parser for defined intervals (on the form [ _, _ ])
                    defIntervalP :: Parsec String () (String, String)
                    defIntervalP = do
                                    char '['
                                    fst <- choice [floatP, infinityP]
                                    char ','
                                    spaces
                                    snd <- choice [floatP, infinityP]
                                    char ']'
                                    many (oneOf ", r(?")
                                    return (fst, snd)
                                    where 
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
                    -- Converts given string to an interval bound; infinite and open or finite (floating point number) and closed
                    bound :: String -> (Extended Float, Boundary)
                    bound str = case str of
                                    "inf"   -> (PosInf, Open)
                                    "-inf"  -> (NegInf, Open)
                                    _       -> case readMaybe str :: Maybe Float of
                                                Just d  -> (Finite d, Closed)
                                                Nothing -> error $ "Failed to parse " ++ str ++ " as floating point number" 
