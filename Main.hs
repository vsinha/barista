module Main where

import BaristaParser
import Test

import Text.ParserCombinators.Parsec

main :: IO ()
main = do 
     print $ parse recipe "" (fst Test.simpleExample)
     --print $ parse recipe "" (fst Test.numberedIngredients)
