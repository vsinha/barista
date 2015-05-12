module Test where

import Control.Applicative
import Data.List
import Test.HUnit
import Text.ParserCombinators.Parsec

import BaristaParser

-- create a parse test given an info message, parser, and tuple of (inputToTest, desiredOuput)
ptest msg prsr testcase = TestCase . either (assertFailure . show) 
                                            (assertEqual msg $ snd testcase)
                             $ (show <$> parse prsr "" (fst testcase))

ptests msg prsr testcases = map (ptest msg prsr) testcases

simpleExample = (
     unlines [
     "Recipe Mocha\r",
     "- 30`mL Steamed_Milk\r",
     "- 60`mL Chocolate\r",
     "- 60`mL Espresso\r"],
     "Recipe {recipeName = \"Mocha\", ingredients = [Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Nothing, annotations = Nothing},Ingredient {volume = 60, measure = Milli, ingredientName = \"Chocolate\", index = Nothing, annotations = Nothing},Ingredient {volume = 60, measure = Milli, ingredientName = \"Espresso\", index = Nothing, annotations = Nothing}]}")

test_simpleInput = ptests "for the first recipe in the language spec" 
                          recipe 
                          [simpleExample]

test_ingredient = ptests "for valid single non-indexed ingredient" 
                         ingredient 
                         [( "- 30`mL Steamed_Milk\r\n", "Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Nothing, annotations = Nothing}"),
                          ( "- 30`mL Steamed_Milk;", "Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Nothing, annotations = Nothing}")]


test_ingredientIndexed = ptests "for valid single indexed ingredient" 
                         ingredient
                         [("1- 30`mL Steamed_Milk\r\n", "Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Just 1, annotations = Nothing}"),
                          ("2- 30`mL Steamed_Milk\r\n", "Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Just 2, annotations = Nothing}")]

{-
-- TODO figure out how to ensure we fail on this
test_ingredientNoEOL = ptests "For invalid single indexed ingredient" 
                       ingredient
                       [("1- 30`mL Steamed_Milk", "")] -- this should fail
-}

test_ingredientAnnotated = ptests "for an ingredient with an annotation"
                         ingredient
                         [( "1- 30`mL Steamed_Milk [mix]\r\n", "Ingredient {volume = 30, measure = Milli, ingredientName = \"Steamed_Milk\", index = Just 1, annotations = Just [Mix]}"),
                          ( "- 60`mL Espresso [mix, mix_source, aspirate_speed, dispense_speed]\r\n", "Ingredient {volume = 60, measure = Milli, ingredientName = \"Espresso\", index = Nothing, annotations = Just [Mix,MixSource,AspirateSpeed,DispenseSpeed]}"),
                          ( "- 60`mL Espresso [hold 5`C]\r\n", "Ingredient {volume = 60, measure = Milli, ingredientName = \"Espresso\", index = Nothing, annotations = Just [Hold 5 C]}")] 

     
numberedIngredients = (unlines [
     "Recipe IcedMocha\r",
     "1- 60`mL Chocolate\r",
     "2- 60`mL Espresso [mix]\r",
     "3- 30`mL Milk\r"],
     "")
----

-- map and concat since each of the "test_testName" are a list of testcases
tests = TestList $ concat [map (TestLabel "Recipe, simple") test_simpleInput
                          ,map (TestLabel "Ingredient, non-indexed") test_ingredient
                          ,map (TestLabel "Ingredient, indexed") test_ingredientIndexed
                          ,map (TestLabel "Ingredient, annotated") test_ingredientAnnotated 
                          ]

runtests = runTestTT tests
