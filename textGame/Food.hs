module Food
( Food(..)
, apple
, noneFood
) where

data Food = Food { foodName :: String
                 , healthPoints :: Int
                 , energyPoints :: Int
} deriving (Show, Eq)

apple = Food "Apple" 15 20

noneFood = Food "" 0 0
