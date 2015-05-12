module Test where

import Control.Applicative
import Test.HUnit
import Test.QuickCheck
import Text.ParserCombinators.Parsec

import BaristaParser

-- create a parse test given an info message, parser, and tuple of (inputToTest, desiredOuput)
ptest msg prsr testcase = TestCase . either (assertFailure . show) 
                                            (assertEqual msg $ snd testcase)
                             $ (show <$> parse prsr "" (fst testcase))

simpleExample = (
     unlines [
     "Recipe Mocha\r",
     "- 30`mL Steamed_Milk\r",
     "- 60`mL Chocolate\r",
     "- 60`mL Espresso\r"],
     "Recipe {recipeName = \"Mocha\", ingredients = [Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = Nothing},Ingredient {volume = 60, measure = \"ml\", ingredientName = \"Chocolate\", index = Nothing},Ingredient {volume = 60, measure = \"ml\", ingredientName = \"Espresso\", index = Nothing}]}")

test_simpleInput = ptest "for the first recipe in the language spec" 
                         recipe 
                         simpleExample

test_ingredient = ptest "for valid single non-indexed ingredient" 
                         ingredient 
                         ( "- 30`mL Steamed_Milk\r\n", "Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = Nothing}")

test_ingredientIndexed = ptest "for valid single indexed ingredient" 
                         ingredient
                         ("1- 30`mL Steamed_Milk\r", "Ingredient {volume = 30, measure = \"ml\", ingredientName = \"Steamed_Milk\", index = 1}")

numberedIngredients = (unlines [
     "Recipe IcedMocha\r",
     "1- 60`mL Chocolate\r",
     "2- 60`mL Espresso [mix]\r",
     "3- 30`mL Milk\r"],
     "")
----
tests = TestList [TestLabel "Recipe, simple" test_simpleInput
                 ,TestLabel "Ingredient, non-indexed" test_ingredient
                 ,TestLabel "Ingredient, indexed" test_ingredientIndexed
                 ]

runtests = runTestTT tests
