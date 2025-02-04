module Main where

import Data.List (sortBy, subsequences)
import Data.Ord (comparing)
import Data.Ratio

candidates :: [(String, Ratio Int)]
candidates =
    [ ("1/6", 1 % 6)
    , ("1/25", 1 % 25)
    , ("3/5", 3 % 5)
    , ("3/20", 3 % 20)
    , ("4/15", 4 % 15)
    , ("5/8", 5 % 8)
    ]

main :: IO ()
main = prettyPrint . head . sortByClosest $ allPermutations candidates

prettyPrint :: [(String, Ratio Int)] -> IO ()
prettyPrint l = do
    print l
    print (sumRatios l)

sumRatios :: [(String, Ratio Int)] -> Ratio Int
sumRatios = sum . map snd

diffFromOne :: Ratio Int -> Ratio Int
diffFromOne r = abs (r - 1)

sortByClosest :: [[(String, Ratio Int)]] -> [[(String, Ratio Int)]]
sortByClosest = sortBy (comparing (diffFromOne . sumRatios))

allPermutations :: [(String, Ratio Int)] -> [[(String, Ratio Int)]]
allPermutations = subsequences
