module Inventory
( Inventory(..)
, FoodItems(..)
, OtherItems(..)
, emptyInventory
, updateFoodInventory
, updateOtherItemsInventory
) where

import Food(Food)

type FoodItems = [Food]
type OtherItems = [String]

-- contains two separate lists: 1 for food items only; 2 for other stuff
data Inventory = Inventory { foodItems :: FoodItems
                          , otherItems :: OtherItems
} deriving (Show, Eq)

emptyInventory = Inventory [][]

updateFoodInventory :: Inventory -> [Food] -> Inventory
updateFoodInventory inventory newFoodItems = Inventory newFoodItems (otherItems inventory)

updateOtherItemsInventory :: Inventory -> [String] -> Inventory
updateOtherItemsInventory inventory newOtherItems = Inventory (foodItems inventory) newOtherItems
