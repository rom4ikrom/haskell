module Player
( Player(..)
, Inventory(..)
, increaseHealth
, reduceHealth
, setHealth
, increaseEnergy
, reduceEnergy
, setEnergy
, updatePosition
, updateInventory
, updateWeapon
) where

import Move
import Weapon(Weapon)
import Food(Food)
import Inventory

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

setHealth :: Player -> Int -> Player
setHealth player x = Player x (energy player) (currentPosition player) (currentInventory player) (currentWeapon player)

increaseEnergy :: Player -> Int -> Player
increaseEnergy player x = Player (health player) ((energy player) + x) (currentPosition player) (currentInventory player) (currentWeapon player)

reduceEnergy :: Player -> Int -> Player
reduceEnergy player x = Player (health player) ((energy player) - x) (currentPosition player) (currentInventory player) (currentWeapon player)

setEnergy :: Player -> Int -> Player
setEnergy player x = Player (health player) x (currentPosition player) (currentInventory player) (currentWeapon player)

updatePosition :: Player -> Move -> Player
updatePosition player direction = Player (health player) (energy player) (move direction (currentPosition player)) (currentInventory player) (currentWeapon player)

updateInventory :: Player -> Inventory -> Player
updateInventory player inventory = Player (health player) (energy player) (currentPosition player) inventory (currentWeapon player)

updateWeapon :: Player -> Weapon -> Player
updateWeapon player weapon = Player (health player) (energy player) (currentPosition player) (currentInventory player) weapon
