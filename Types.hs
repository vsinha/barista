module Types where

data Recipe = Recipe
     { recipeName :: String
     , ingredients :: [Ingredient]
     } deriving Show

data Ingredient = Ingredient 
     { volume :: Int
     , measure :: Measure
     , ingredientName :: String
     , index :: (Maybe Int)
     } deriving Show

type Measure = String

