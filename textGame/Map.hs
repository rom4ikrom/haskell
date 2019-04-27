module Map
( Map (..)
, Pos(..)
, Room(..)
, Item(..)
, generateMap
, updateItemsMap
) where

import Move
type Room = String
type Item = String

pathsmap :: [(Pos, Room)]
pathsmap = [
  --ground floor
  ((1,1,0), "room1"),
  ((2,1,0), "room2"),
  ((3,1,0), "room3"),
  ((1,2,0), "room4"),
  ((2,2,0), "room5"),
  ((3,2,0), "room6"),
  --first floow
  ((1,1,1), "room1A"),
  ((2,1,1), "room2A"),
  ((3,1,1), "room3A"),
  ((1,2,1), "room4A"),
  ((2,2,1), "room5A"),
  ((3,2,1), "room6A"),
  --basement floor
  ((1,1,-1), "room1B"),
  ((2,1,-1), "room2B"),
  ((3,1,-1), "room3B"),
  ((1,2,-1), "room4B"),
  ((2,2,-1), "room5B"),
  ((3,2,-1), "room6B")
  ]

itemsmap :: [(Pos, Item)]
itemsmap = [
  ((3,1,0), "apple"),
  ((3,2,0), "sword"),
  ((2,2,0), "enemy"),
  ((1,2,0), "enemy")
  ]

data Map = Map { pathsMap :: [(Pos, Room)]
               , itemsMap :: [(Pos, Item)]
               }
generateMap :: Map
generateMap = Map pathsmap itemsmap

updateItemsMap :: Map -> [(Pos, Item)] -> Map
updateItemsMap worldMap newItemsMap = Map (pathsMap worldMap) newItemsMap















  --
