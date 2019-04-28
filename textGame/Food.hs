module Food
( Food(..)
, apple
, chocolate
, steak
, spoiledMilk
, noneFood
) where

data Food = Food { foodName :: String
                 , healthPoints :: Int
                 , energyPoints :: Int
} deriving (Show, Eq)

apple = Food "Apple" 18 10

chocolate = Food "Chocolate" 23 30

steak = Food "Steak" 50 20

spoiledMilk = Food "Spoiled Milk" (-10) (-5)

noneFood = Food "" 0 0
