module Map
( Map (..)
, Room(..)
, Food(..)
, generateMap
, updateFoodMap
, updateWeaponMap
, updateEnemyMap
) where

import Move (Pos)
import Food
import Weapon
import Enemy
type Room = String


pathsmap :: [(Pos, Room)]
pathsmap = [
  --ground floor - go up from room2 and room6 | go down from room3
  ((1,1,0), "room1"),
  ((2,1,0), "room2"),
  ((3,1,0), "room3"),
  ((1,2,0), "room4"),
  ((2,2,0), "room5"),
  ((3,2,0), "room6"),
  --first floor - go down from room2 and room6
  ((1,1,1), "room1A"),
  ((2,1,1), "room2A"),
  ((3,1,1), "room3A"),
  ((1,2,1), "room4A"),
  ((2,2,1), "room5A"),
  ((3,2,1), "room6A"),
  --basement floor - go up from rooom3
  ((1,1,-1), "room1B"),
  ((2,1,-1), "room2B"),
  ((3,1,-1), "room3B"),
  ((1,2,-1), "room4B"),
  ((2,2,-1), "room5B"),
  ((3,2,-1), "room6B")
  ]

foodmap :: [(Pos, Food)]
foodmap = [
  ((3,1,0), apple), -- room3
  ((2,1,-1), steak), -- room2B
  ((2,2,1), chocolate) -- room5A
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

data Map = Map { pathsMap :: [(Pos, Room)]
               , foodMap :: [(Pos, Food)]
               , weaponMap :: [(Pos, Weapon)]
               , enemyMap :: [(Pos, Enemy)]
               }
generateMap :: Map
generateMap = Map pathsmap foodmap weaponmap enemymap

updateFoodMap :: Map -> [(Pos, Food)] -> Map
updateFoodMap worldMap newFoodMap = Map (pathsMap worldMap) newFoodMap (weaponMap worldMap) (enemyMap worldMap)

updateWeaponMap :: Map -> [(Pos, Weapon)] -> Map
updateWeaponMap worldMap newWeaponMap = Map (pathsMap worldMap) (foodMap worldMap) newWeaponMap (enemyMap worldMap)

updateEnemyMap :: Map -> [(Pos, Enemy)] -> Map
updateEnemyMap worldMap newEnemyMap = Map (pathsMap worldMap) (foodMap worldMap) (weaponMap worldMap) newEnemyMap















  --
