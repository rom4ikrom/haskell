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

data Inventory = Inventory { foodItems :: FoodItems
                          , otherItems :: OtherItems
} deriving (Show, Eq)

emptyInventory = Inventory [][]

updateFoodInventory :: Inventory -> [Food] -> Inventory
updateFoodInventory inventory newFoodItems = Inventory newFoodItems (otherItems inventory)

updateOtherItemsInventory :: Inventory -> [String] -> Inventory
updateOtherItemsInventory inventory newOtherItems = Inventory (foodItems inventory) newOtherItems
