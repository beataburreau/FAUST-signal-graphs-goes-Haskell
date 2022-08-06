module GraphMonad (GraphM, runGraphM, State, initState, Error(..), Edge(..), Node(..), Primitive(..), Operator(..), Math(..), UI(..),
    ComputationRate(..), Type(..), SFix(..), toPrimitive, toComputationRate, toType, parseLabel, lookupNode, lookupEdge, setLSB, 
    setPathSuccs, addPathSucc, removePathSucc, getLastPathSucc, cleanShow) where

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
import Sfix (maxLSB)


-- The graph monad
-- Keeps node and edge information in the state while allowing IO action and exception handling
type GraphM a = StateT State (ExceptT Error IO) a

runGraphM :: State -> GraphM a -> IO (Either Error (a, State))
runGraphM s = runExceptT . flip runStateT s

data Error = ParseError String
           | NoSuchFile FilePath
           | GraphError String
           | InferenceError String
instance Exception Error

instance Show Error where
    show (ParseError e) =
        "syntax error:\n" ++ e
    show (NoSuchFile f) =
        "file not found: " ++ f
    show (GraphError e) = 
        "graph error:\n" ++ e
    show (InferenceError e) = 
        "inference error:\n" ++ e

-- The State
-- Holds a tuple of additional graph information; 
-- node information in the first tuple element and edge information in the second
type State = ([Node Int], [Edge Int])

-- Inital state: an empty state
initState :: State
initState = ([],[])

-- Node data type
data Node ni = Node {
    ni         :: Int,         -- unique integer indentifier
    nid        :: String,      -- identifier in .dot graph, e.g. S0x7fb9cfd0abe0
    ntype      :: Type,
    nprimitive :: Primitive,
    nrate      :: ComputationRate,
    npathsuccs :: [Int]        -- successors on LSB inference path, 
                               -- the head of the list is the nid of the most recent successor 
}

instance Eq (Node ni) where 
    n1 == n2 = 
        ni n1  == ni n2  &&
        nid n1 == nid n2

instance Show (Node ni) where
    show (Node i id t p r succs) = show i ++ ": \"" ++ id ++ "\", " ++ show t ++ ", " ++ show p ++ ", " ++ show r ++ ", " ++ show succs

-- Edge data type
data Edge ei = Edge {
    ei          :: Int,         -- unique integer indentifier 
    ei1         :: Int,         -- start node identifier 
    ei2         :: Int,         -- end node identifier
    etype       :: Type,
    earg        :: Int,
    einterval   :: Maybe (Interval Float),
    esfix       :: SFix
}

instance Eq (Edge ei) where 
    e1 == e2 = 
        ei e1  == ei e2  && 
        ei1 e1 == ei1 e2 &&
        ei2 e1 == ei2 e2

instance Show (Edge ei) where
    show (Edge i i1 i2 t a interval sfix) = show i1 ++ "->" ++ show i2 ++ ":" ++ ", " ++ show t ++ ", " ++ show a ++ ", " ++ show interval ++ "," ++ show sfix

-- Signed fixpoint format: sfix(msb, lsb)
data SFix = SFix {
    msb :: Maybe Int, 
    lsb :: Maybe Int, 
    trimmed :: Bool
}

instance Show SFix where 
    show (SFix (Just msb) (Just lsb) _) = "sfix(" ++ show msb ++ ", " ++ show lsb ++ ")" 
    show (SFix  Nothing   (Just lsb) _) = "sfix(?, " ++ show lsb ++ ")" 
    show (SFix (Just msb)  Nothing   _) = "sfix(" ++ show msb ++ ", ?)" 
    show _                              = "sfix(?, ?)"

data Type = Integer
          | Float
    deriving Show

data Primitive = Number (Either Int Float) -- a number is _either_ an integer or a floating point number
               | Operator Operator
               | Math Math
               | UI UI
               | Proj Int
               | Rec Int
               | FSamplingFreq 
               | Input Int
               | Output

instance Show Primitive where
    show (Number (Left i))  = show i
    show (Number (Right f)) = show f
    show (Operator op)      = show op
    show (Math m)           = show m
    show (UI ui)            = show ui
    show (Proj i)           = "Proj" ++ show i
    show (Rec i)            = "REC W" ++ show i
    show FSamplingFreq      = "fSamplingFreq"
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

data Math = ACos | ASin | ATan | ATan2 | Cos | Sin | Tan | Exp | Log | Log10 | MPow | Sqrt | Abs | Min | Max | FMod | Remainder | Floor | Ceil | Rint

instance Show Math where 
    show ACos       = "acos"
    show ASin       = "asin"
    show ATan       = "atan"
    show ATan2      = "atan2"
    show Cos        = "cos"
    show Sin        = "sin"
    show Tan        = "tan"
    show Exp        = "exp"
    show Log        = "log"
    show Log10      = "log10"
    show MPow       = "pow"
    show Sqrt       = "sqrt"
    show Abs        = "abs"
    show Min        = "min"
    show Max        = "max"
    show FMod       = "fmod"
    show Remainder  = "remainder"
    show Floor      = "floor"
    show Ceil       = "ceil"
    show Rint       = "rint"

data UI = Button | Checkbox | HSlider Float | VSlider Float | Nentry Float
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
                | isJust math                               = Math $ fromJust math
                | isJust ui                                 = UI $ fromJust ui
                | enumeratedStr 4 "Proj" 4                  = Proj $ fromLeft (throw $ ParseError "Invalid projection number, it must be an integer value") (fromJust $ enumeration 4)
                | enumeratedStr 5 "REC W" 5                 = Rec $ fromLeft (throw $ ParseError "Invalid projection number, it must be an integer value") (fromJust $ enumeration 5)
                | str == "fSamplingFreq"                    = FSamplingFreq    
                | enumeratedStr 5 "INPUT" 6                 = Input $ fromLeft (throw $ ParseError "Invalid input number, it must be an integer value") (fromJust $ enumeration 6)
                | str == "OUTPUT_0"                         = Output
                | otherwise                                 = throw $ ParseError $ str ++ " is not a valid primitive value; it must be an integer, float, operator, UI element, projection, input or output signal"
                where
                    number = toNumber str
                    operator = toOperator str
                    math = toMath str
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
                                     "fmod"  -> Just Mod
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
                    toMath :: String -> Maybe Math
                    toMath str = case str of 
                                "acos"      -> Just ACos
                                "asin"      -> Just ASin
                                "atan"      -> Just ATan
                                "atan2"     -> Just ATan2
                                "cos"       -> Just Cos
                                "sin"       -> Just Sin
                                "tan"       -> Just Tan
                                "exp"       -> Just Exp
                                "log"       -> Just Log
                                "log10"     -> Just Log10
                                "pow"       -> Just MPow
                                "sqrt"      -> Just Sqrt
                                "abs"       -> Just Abs
                                "min"       -> Just Min
                                "max"       -> Just Max
                                "fmod"      -> Just FMod
                                "remainder" -> Just Remainder
                                "floor"     -> Just Floor
                                "ceil"      -> Just Ceil
                                "rint"      -> Just Rint
                                _           -> Nothing
                    toUI :: String -> Maybe UI
                    toUI str = case str of
                                "button"   -> Just Button
                                "checkbox" -> Just Checkbox
                                _          -> case parse uiP "" str of 
                                                    Left _           -> Nothing
                                                    Right (ui, step) -> case readMaybe step :: Maybe Float of
                                                                            Just d  -> case ui of 
                                                                                        "hslider" -> Just $ HSlider d
                                                                                        "vslider" -> Just $ VSlider d
                                                                                        "nentry"  -> Just $ Nentry d
                                                                                        _         -> throw $ ParseError $ ui ++ " is not a valid UI primitive"
                                                                            Nothing -> throw $ ParseError $ "Failed to parse " ++ step ++ " as floating point number"
                                where 
                                    uiP :: Parsec String () (String, String)
                                    uiP = do 
                                          ui <- choice [string "hslider", string "vslider", string "nentry"]
                                          string ", step("
                                          step <- floatP
                                          return (ui, step)



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
                                    fstsign <- option "" (string "-")
                                    fst <- choice [floatP, infinityP]
                                    char ','
                                    spaces
                                    sndsign <- option "" (string "-")
                                    snd <- choice [floatP, infinityP]
                                    char ']'
                                    many (oneOf ", r(?")
                                    return (fstsign ++ fst, sndsign ++ snd)
                                    where
                                    -- Parsec parser for infinities
                                    infinityP :: Parsec String () String
                                    infinityP = do
                                                string "inf"
                                                return "inf"
                    -- Converts given string to an interval bound; infinite and open or finite (floating point number) and closed
                    bound :: String -> (Extended Float, Boundary)
                    bound str = case str of
                                    "inf"   -> (PosInf, Open)
                                    "-inf"  -> (NegInf, Open)
                                    _       -> case readMaybe str :: Maybe Float of
                                                Just d  -> (Finite d, Closed)
                                                Nothing -> throw $ ParseError $ "Failed to parse " ++ str ++ " as floating point number"

-- Parsec parser for floating point numbers
floatP :: Parsec String () String
floatP = do
            intPart <- manyTill digit (char '.')
            decPart <- many1 digit
            return $ intPart ++ "." ++ decPart


-- Finds the node with given integer identifier, 
-- if the node is present in the node list
lookupNode :: Int -> [Node Int] -> Maybe (Node Int)
lookupNode _ []     = Nothing
lookupNode i (n:ns) = if i == ni n
                          then Just n
                          else lookupNode i ns

-- Finds the edge spanning between the two nodes with given integer identifiers, 
-- if the edge is present in the edge list
lookupEdge :: Int -> Int -> [Edge Int] -> Maybe (Edge Int)
lookupEdge _  _  []     = Nothing
lookupEdge i1 i2 (e:es) = if i1 == ei1 e && i2 == ei2 e
                          then Just e
                          else lookupEdge i1 i2 es

setLSB :: Int -> Edge Int -> Edge Int
setLSB newlsb e@(Edge _ _ _ _ _ _ (SFix msb lsb _)) = let newlsb' = if newlsb >= maxLSB then newlsb else maxLSB
                                                      in case lsb of
                                                              Nothing -> e {esfix = SFix msb (Just newlsb') False}
                                                              Just _  -> e {esfix = SFix msb (Just newlsb') True}

setPathSuccs :: [Int] -> Node Int -> Node Int
setPathSuccs succs n = n {npathsuccs = succs}

-- Adds given node to list of path successors
addPathSucc :: Int -> Node Int -> Node Int
addPathSucc succ n = n {npathsuccs = succ : npathsuccs n}

-- Gives the last path successor
getLastPathSucc :: Node Int -> Maybe Int
getLastPathSucc n = case npathsuccs n of 
                        []    -> Nothing
                        succs -> Just $ last succs

-- Removes the last path successor from the nodes successor list
removePathSucc :: Node Int -> Node Int
removePathSucc n = n {npathsuccs = tail $ npathsuccs n}

-- For debugging purposes
cleanShow :: Edge Int -> String
cleanShow e = show (ei1 e) ++ " -> " ++ show (ei2 e)