module MSBs (getMSB) where

-- LIBRARIES
import Data.Interval (Interval, upperBound, lowerBound, Extended (NegInf, PosInf, Finite))
import qualified Data.ExtendedReal as ER(isInfinite)
import Sfix (defaultMSB)


getMSB :: Maybe (Interval Float) -> Int
getMSB (Just intrvl) = case lowerBound intrvl of 
                        Finite lb -> case upperBound intrvl of 
                            Finite ub -> msb $ max lb ub
                            _ -> defaultMSB
                        _ -> defaultMSB
getMSB Nothing       = defaultMSB
                        
msb :: Float -> Int
msb = (+ 1) . length . toBinary . toInt 

toInt :: Float -> Int
toInt = ceiling 

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (helper n)
            where 
                helper 0 = []
                helper n = let (q,r) = n `divMod` 2 in r : helper q