module Map
( Map (..)
, Room(..)
, Food(..)
, generateMap
, updatePathsMap
, updateFoodMap
, updateWeaponMap
, updateEnemyMap
, updateItemsMap
) where

import Move (Pos)
import Food
import Weapon
import Enemy
type Room = String
type Thing = String

pathsmap :: [(Pos, Room)]
pathsmap = [
  --ground floor - go up from room2 and room6 | go down from room3
  ((1,1,0), "Room 1"),
  ((2,1,0), "Room 2"),
  ((3,1,0), "Room 3"),
  ((1,2,0), "Room 4"),
  ((2,2,0), "Room 5"),
  ((3,2,0), "Room 6"),
  --first floor - go down from room2 and room6
  ((1,1,1), "Room 1A"),
  ((2,1,1), "Room 2A"),
  ((3,1,1), "Room 3A"),
  ((1,2,1), "Room 4A"),
  ((2,2,1), "Room 5A"),
  ((3,2,1), "Room 6A"),
  --basement floor - go up from rooom3
  ((1,1,-1), "Room 1B"),
  ((2,1,-1), "Room 2B"),
  ((3,1,-1), "Room 3B"),
  ((1,2,-1), "Room 4B"),
  ((2,2,-1), "Room 5B"),
  ((3,2,-1), "Room 6B")
  ]

foodmap :: [(Pos, Food)]
foodmap = [
  ((3,1,0), apple), -- room3
  ((2,1,-1), steak), -- room2B
  ((3,1,1), chocolate), -- room3A
  ((3,2,-1), spoiledMilk) -- room6B
  ]

weaponmap :: [(Pos, Weapon)]
weaponmap = [
  ((3,2,0), sword), -- room6
  ((1,1,1), pistol) -- room1A
  ]

enemymap :: [(Pos, Enemy)]
enemymap = [
  ((2,2,0), goblin), -- room5
  ((1,2,0), zombie), -- room4
  ((1,1,-1), goblin), -- room1B
  ((1,2,1), monster) -- room4A
  ]

itemsmap :: [(Pos, Thing)]
itemsmap = [
  ((2,2,-1), "Chest") -- room 5B
  ]

data Map = Map { pathsMap :: [(Pos, Room)]
               , foodMap :: [(Pos, Food)]
               , weaponMap :: [(Pos, Weapon)]
               , enemyMap :: [(Pos, Enemy)]
               , itemsMap :: [(Pos, Thing)]
               }
generateMap :: Map
generateMap = Map pathsmap foodmap weaponmap enemymap itemsmap

updatePathsMap :: Map -> [(Pos, Room)] -> Map
updatePathsMap worldMap newPathsMap = Map newPathsMap (foodMap worldMap) (weaponMap worldMap) (enemyMap worldMap) (itemsMap worldMap)

updateFoodMap :: Map -> [(Pos, Food)] -> Map
updateFoodMap worldMap newFoodMap = Map (pathsMap worldMap) newFoodMap (weaponMap worldMap) (enemyMap worldMap) (itemsMap worldMap)

updateWeaponMap :: Map -> [(Pos, Weapon)] -> Map
updateWeaponMap worldMap newWeaponMap = Map (pathsMap worldMap) (foodMap worldMap) newWeaponMap (enemyMap worldMap) (itemsMap worldMap)

updateEnemyMap :: Map -> [(Pos, Enemy)] -> Map
updateEnemyMap worldMap newEnemyMap = Map (pathsMap worldMap) (foodMap worldMap) (weaponMap worldMap) newEnemyMap (itemsMap worldMap)

updateItemsMap :: Map -> [(Pos, Thing)] -> Map
updateItemsMap worldMap newItemsMap = Map (pathsMap worldMap) (foodMap worldMap) (weaponMap worldMap) (enemyMap worldMap) newItemsMap

















  --
