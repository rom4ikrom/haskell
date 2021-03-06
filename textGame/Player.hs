module Player
( Player(..)
, Inventory(..)
, increaseHealth
, reduceHealth
, increaseEnergy
, reduceEnergy
, setHealthEnergy
, updatePosition
, updateInventory
, updateWeapon
) where

import Move
import Weapon(Weapon)
import Food(Food)
import Inventory

-- the Player data holds inside health, enegy, postion and two complex data types Inventory and Weapon
data Player = Player { health :: Int
                     , energy :: Int
                     , currentPosition :: Pos
                     , currentInventory :: Inventory
                     , currentWeapon :: Weapon
} deriving (Show, Eq)

increaseHealth :: Player -> Int -> Player
increaseHealth player x = Player ((health player) + x) (energy player) (currentPosition player) (currentInventory player) (currentWeapon player)

reduceHealth :: Player -> Int -> Player
reduceHealth player x = Player ((health player) - x) (energy player) (currentPosition player) (currentInventory player) (currentWeapon player)

increaseEnergy :: Player -> Int -> Player
increaseEnergy player x = Player (health player) ((energy player) + x) (currentPosition player) (currentInventory player) (currentWeapon player)

reduceEnergy :: Player -> Int -> Player
reduceEnergy player x = Player (health player) ((energy player) - x) (currentPosition player) (currentInventory player) (currentWeapon player)

setHealthEnergy :: Player -> Int -> Int -> Player
setHealthEnergy player x y = Player x y (currentPosition player) (currentInventory player) (currentWeapon player)

updatePosition :: Player -> Move -> Player
updatePosition player direction = Player (health player) (energy player) (move direction (currentPosition player)) (currentInventory player) (currentWeapon player)

updateInventory :: Player -> Inventory -> Player
updateInventory player inventory = Player (health player) (energy player) (currentPosition player) inventory (currentWeapon player)

updateWeapon :: Player -> Weapon -> Player
updateWeapon player weapon = Player (health player) (energy player) (currentPosition player) (currentInventory player) weapon
