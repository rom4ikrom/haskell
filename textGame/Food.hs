module Food
( Food(..)
, apple
, chocolate
, steak
, noneFood
) where

data Food = Food { foodName :: String
                 , healthPoints :: Int
                 , energyPoints :: Int
} deriving (Show, Eq)

apple = Food "Apple" 15 20

chocolate = Food "Chocolate" 20 30

steak = Food "Steak" 50 20

noneFood = Food "" 0 0
