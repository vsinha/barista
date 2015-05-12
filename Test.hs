module Test where

import Control.Applicative
import Test.HUnit
import Test.QuickCheck
import Text.ParserCombinators.Parsec

import BaristaParser

pTest msg e = TestCase . either (assertFailure . show) (assertEqual msg e)

runParse r e = show <$> parse r "" e

numberedIngredients = unlines [
     "Recipe IcedMocha\r",
     "1- 60`mL Chocolate\r",
     "2- 60`mL Espresso [mix]\r",
     "3- 30`mL Milk\r"]

----
simpleExample = unlines [
     "Recipe Mocha\r",
     "- 30`mL Steamed_Milk\r",
     "- 60`mL Chocolate\r",
     "- 60`mL Espresso\r"]

simpleExample_output = 
     "Recipe {recipeName = \"Mocha\", ingredients = [Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = Nothing},Ingredient {volume = 60, measure = \"ml\", ingredientName = \"Chocolate\", index = Nothing},Ingredient {volume = 60, measure = \"ml\", ingredientName = \"Espresso\", index = Nothing}]}"

test_simpleInput = pTest "for the first recipe in the language spec" 
     simpleExample_output $ runParse recipe simpleExample

----
singleIngredientNoIndex = "- 30`mL Steamed_Milk\r\n"

singleIngredientNoIndex_output = "Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = Nothing}"

test_ingredient = pTest "for valid single non-indexed ingredient" 
     singleIngredientNoIndex_output $ runParse ingredient singleIngredientNoIndex

----
singleIngredientIndexed = "1- 30`mL Steamed_Milk\r\n"

singleIngredientIndexed_output = "Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = 1}"

test_ingredientIndexed = pTest "for valid single indexed ingredient" 
     singleIngredientIndexed_output $ runParse ingredient singleIngredientIndexed

tests = TestList [TestLabel "Recipe, simple" test_simpleInput
                 ,TestLabel "Ingredient, non-indexed" test_ingredient
                 ,TestLabel "Ingredient, indexed" test_ingredientIndexed
                 ]

runtests = runTestTT tests
