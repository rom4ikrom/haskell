module Player
( Player(..)
, Inventory(..)
, reduceHealth
, reduceEnergy
, updatePosition
, updateInventory
) where

import Move

type Inventory = [String]

data Player = Player { health :: Int
                     , energy :: Int
                     , currentPosition :: Pos
                     , currentInventory :: Inventory
} deriving (Show, Eq)

reduceHealth :: Player -> Int -> Player
reduceHealth player x = Player ((health player) - x) (energy player) (currentPosition player) (currentInventory player)

reduceEnergy :: Player -> Player
reduceEnergy player = Player (health player) ((energy player) - 1) (currentPosition player) (currentInventory player)

updatePosition :: Player -> Move -> Player
updatePosition player direction = Player (health player) (energy player) (move direction (currentPosition player)) (currentInventory player)

updateInventory :: Player -> Inventory -> Player
updateInventory player inventory = Player (health player) (energy player) (currentPosition player) inventory

--
