{-# LANGUAGE RankNTypes #-}

module GraphMonad
    (GraphM,
    runGraphM,
    State,
    initState,
    Error(..),
    Edge(Edge),
    Node(Node, nid, ni),
    Primitive(..),
    ComputationRate(..),
    Type(..),
    toPrimitive,
    toComputationRate,
    toType,
    parseLabel)
where

-- MONADS
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Exception (Exception, throw)

-- LIBRARIES
import Data.Interval (Interval, Boundary (..), interval, Extended (NegInf, PosInf, Finite))
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Text.Parsec (Parsec, parse, oneOf, manyTill, string, digit, spaces, many1, many, option, choice, char, ParseError, anyChar)
import Numeric (readHex)
import Data.Char (toLower)
import Data.Either (fromLeft, fromRight)


-- The graph monad
-- Keeps node and edge information in the state while allowing IO action and exception handling
type GraphM a = StateT State (ExceptT Error IO) a

runGraphM :: State -> GraphM a -> IO (Either Error (a, State))
runGraphM s = runExceptT . flip runStateT s

data Error = ParseError String
           | NoSuchFile FilePath
instance Exception Error

instance Show Error where
  show (ParseError e) =
    "syntax error:\n" ++ e
  show (NoSuchFile f) =
    "file not found: " ++ f

-- The State
-- Holds a tuple of additional graph information; 
-- node information in the first tuple element and edge information in the second
type State = ([Node Int], [Edge Int])

-- Inital state: an empty state
initState :: State
initState = ([],[])

-- Node data type
data Node ni = Node {
    ni :: Int,      -- unique integer indentifier
    nid :: String,  -- identifier in .dot graph, e.g. S0x7fb9cfd0abe0
    ntype :: Type,
    nprimitive :: Primitive,
    nrate :: ComputationRate
}

instance Show (Node ni) where
    show (Node i id t p r) = show i ++ ": \"" ++ show id ++ "\", " ++ show t ++ ", " ++ show p ++ ", " ++ show r

-- Edge data type
data Edge ei = Edge {
    ei :: Int,      -- unique integer indentifier 
    ei1 :: Int,     -- start node identifier 
    ei2 :: Int,     -- end node identifier
    etype :: Type,
    earg :: Int,
    einterval :: Maybe (Interval Float)
}

instance Show (Edge ei) where
    show (Edge i i1 i2 t a interval) = show i1 ++ "->" ++ show i2 ++ ":" ++ ", " ++ show t ++ ", " ++ show a ++ ", " ++ show interval

data Type = Integer
          | Float
    deriving Show

data Primitive = Number (Either Int Float) -- a number is _either_ an integer or a floating point number
               | Operator Operator
               | UI UI
               | Proj Int
               | Rec Int
               | Input Int
               | Output

instance Show Primitive where
    show (Number (Left i))  = show i
    show (Number (Right f)) = show f
    show (Operator op)      = show op
    show (UI ui)            = show ui
    show (Proj i)           = "Proj" ++ show i
    show (Rec i)            = "REC W" ++ show i
    show (Input i)          = "INPUT " ++ show i
    show Output             = "OUTPUT"


data Operator = IntCast | FloatCast | Cut | Delay | Pow | Mul | Div | Mod | Add | Sub | LT | LE | GT | GE | EQ | NEQ | XOR | AND | OR | LShift | RShift

instance Show Operator where
    show IntCast        = "(int)"
    show FloatCast      = "(float)"
    show Cut            = "!"
    show Delay          = "@"
    show Pow            = "^"
    show Mul            = "*"
    show Div            = "/"
    show Mod            = "%"
    show Add            = "+"
    show Sub            = "-"
    show GraphMonad.LT  = "<"
    show LE             = "<="
    show GraphMonad.GT  = ">"
    show GE             = ">="
    show GraphMonad.EQ  = "=="
    show NEQ            = "!="
    show XOR            = "xor"
    show AND            = "&"
    show OR             = "|"
    show LShift         = "<<"
    show RShift         = ">>"


data UI = Button | Checkbox | HSlider | VSlider | Nentry
    deriving Show

data ComputationRate = Constant
                     | Sample
                     | Block
    deriving Show

-- Converts a .dot graph color into the corresponding type
toType :: String -> Type
toType str = case str of
                "blue"  -> Integer
                "blue2" -> Integer
                "red"   -> Float
                "red2"  -> Float
                _       -> throw $ ParseError "Not a valid type"

-- Converts a .dot graph node label into the corresponding primitive
toPrimitive :: String -> Primitive
toPrimitive str | isJust number                             = Number $ fromJust number
                | isJust operator                           = Operator $ fromJust operator
                | isJust ui                                 = UI $ fromJust ui
                | enumeratedStr 4 "Proj" 4                  = Proj $ fromLeft (throw $ ParseError "Invalid projection number, it must be an integer value") (fromJust $ enumeration 4)
                | enumeratedStr 5 "REC W" 5                 = Rec $ fromLeft (throw $ ParseError "Invalid projection number, it must be an integer value") (fromJust $ enumeration 5)
                | enumeratedStr 5 "INPUT" 6                 = Input $ fromLeft (throw $ ParseError "Invalid input number, it must be an integer value") (fromJust $ enumeration 6)
                | str == "OUTPUT_0"                         = Output
                | otherwise                                 = throw $ ParseError $ str ++ " is not a valid primitive value; it must be an integer, float, operator, UI element, projection, input or output signal"
                where
                    number = toNumber str
                    operator = toOperator str
                    ui = toUI str
                    enumeratedStr :: Int -> String -> Int -> Bool
                    enumeratedStr t substr d = take t str == substr && isJust (enumeration d)
                    enumeration :: Int -> Maybe (Either Int Float)
                    enumeration d = toNumber $ drop d str
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
                                     "<"     -> Just GraphMonad.LT
                                     "<="    -> Just LE
                                     ">"     -> Just GraphMonad.GT
                                     ">="    -> Just GE
                                     "=="    -> Just GraphMonad.EQ
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
                            "box"     -> Constant
                            "ellipse" -> Sample
                            "hexagon" -> Block
                            _         -> throw $ ParseError "Not a valid computation rate"

-- Parses an edge label (string) into a tuple of an int and a floating point interval using Parsec parsers
parseLabel :: String -> (Int, Maybe (Interval Float))
parseLabel str = case parse argP "" str of
                    Left  _          -> throw $ ParseError $ "Failed to parse label " ++ str ++ ", expected format \"arg(n), [l, u], r(???)\""
                    Right (arg, rem) -> (read arg, parseInterval rem)
                where
                    -- Parsec parser for argument order n (on the form arg(n))
                    argP :: Parsec String () (String, String)
                    argP = do
                            string "arg("
                            n <- many1 digit
                            string "),"
                            spaces
                            rem <- many anyChar
                            return (n, rem)

-- Parses a string into a floating point interval using Parsec parsers
-- EX/ 
-- "[4.000000, inf], r(???)" is successfully parsed as a defined interval, giving Just (Finite 4.00 <=..< PosInf)
-- "[???], r(???)" is successfully parsed as an undefined interval,        giving Nothing
parseInterval :: String -> Maybe (Interval Float)
parseInterval str = case parse undefIntervalP "" str of
                    Right _ -> Nothing
                    Left  _ -> case parse defIntervalP "" str of
                                Right (lower, upper) -> Just $ interval (bound lower) (bound upper)
                                Left _               -> throw $ ParseError $ str ++ " is not a valid interval"
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
                                                Nothing -> throw $ ParseError $ "Failed to parse " ++ str ++ " as floating point number"
