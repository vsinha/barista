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
     , annotations :: (Maybe [Action])
     } deriving Show

data Action = Mix 
            | MixSource 
            | Hold Degrees TempUnit
            | AspirateSpeed 
            | DispenseSpeed 
  deriving (Eq, Show)

type Degrees = Int

data Measure = Liter | Milli | Micro 
  deriving (Eq, Ord, Enum, Show)

data TempUnit = C | F | K
  deriving (Eq, Show)
