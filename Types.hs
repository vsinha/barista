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
     , annotations :: (Maybe [Annotation])
     } deriving Show

type Measure = String
data Annotation = MixSource | Hold | AspirateSpeed | DispenseSpeed 
  deriving (Eq, Ord, Enum, Show)

