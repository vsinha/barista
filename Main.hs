module Main where

import BaristaParser
import Text.ParserCombinators.Parsec

simpleExample = unlines [
     "Recipe Mocha\r",
     "- 30`mL Steamed_Milk\r",
     "- 60`mL Chocolate\r",
     "- 60`mL Espresso\r"]

main :: IO ()
main = print $ parse recipe "" simpleExample
