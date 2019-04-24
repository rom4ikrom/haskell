import Data.List

type Pos = (Int, Int)

type Location = String
type Thing = String
type Response = String

type PathMap = [(Pos, Location)]
paths :: PathMap
paths = [
  ((1,1), "room1"),
  ((2,1), "room2"),
  ((3,1), "room3"),
  ((1,2), "room4"),
  ((2,2), "room5"),
  ((3,2), "room6")
  ]

type LocationMap = [(Thing, Pos)]
locations :: LocationMap
locations = [
  ("me", (1,1))
  ]

type ItemsLocationMap = [(Pos, Thing)]
itemsmap :: ItemsLocationMap
itemsmap = [
  ((3,1), "apple"),
  ((3,2), "sword"),
  ((2,2), "enemy"),
  ((1,2), "enemy")
  ]

type ItemNo = Int
type Name = String

type Inventory = [String]
items :: Inventory
items = []

type World = (PathMap, LocationMap, ItemsLocationMap, Inventory, Response)
world :: IO (PathMap, LocationMap, ItemsLocationMap, Inventory, Response)
world = return (paths, locations, itemsmap, items, "")

data Move = North | South | West | East
move :: Move -> Pos -> Pos
move North (x, y) = checkMove (x, y + 1)
move South (x, y) = checkMove (x, y - 1)
move West (x, y) = checkMove (x - 1, y)
move East (x, y) = checkMove (x + 1, y)

checkMove :: Pos -> Pos
checkMove (x, y) = do
  if x == 4 then (x - 1, y)
  else if x == 0 then (x + 1, y)
  else if y == 3 then (x, y - 1)
  else if y == 0 then (x, y + 1)
  else (x, y)

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go in that direction.\n" ++
    "quit               -- to end the game and quit."

main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!"
          putStrLn instructions
          play (return (paths, locations, itemsmap, items, ""))
          return "Bye!"

play :: IO (World) -> IO (World)
play world = do
  (paths, locations, itemsmap, items, response) <- world
  putStrLn response
  if response == "You died! Game Over!" then return (paths, locations, itemsmap, items, "Quitting.")
  else do
    putStr "command> "
    command <- getLine
    if command == "quit" then return (paths, locations, itemsmap, items, "Quitting.")
    else play (return (do_command command paths locations itemsmap items))


-- "get" finds the value of a key in a (key, value) list
getPos :: Eq a => a -> [(a, Pos)] -> Pos
getPos value list = case lookup value list of
                     Just result -> result
                     Nothing -> (0,0)

getValue :: Eq a => a -> [(a, String)] -> String
getValue value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not Found"

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

do_command :: String -> PathMap -> LocationMap -> ItemsLocationMap -> Inventory -> World
do_command "n" paths locations itemsmap items = go North      paths locations itemsmap items
do_command "s" paths locations itemsmap items = go South      paths locations itemsmap items
do_command "w" paths locations itemsmap items = go West       paths locations itemsmap items
do_command "e" paths locations itemsmap items = go East       paths locations itemsmap items
do_command "i" paths locations itemsmap items = showInventory paths locations itemsmap items
do_command "p" paths locations itemsmap items = pickItem      paths locations itemsmap items
do_command "k" paths locations itemsmap items = killEnemy     paths locations itemsmap items
do_command _ paths locations itemsmap items = (paths, locations, itemsmap, items, "Invalid Input!")

addSpace :: String -> String
addSpace xs = if length xs <= 4
              then xs
              else take 4 xs ++ " " ++ addSpace (drop 4 xs)

showInventory :: PathMap -> LocationMap -> ItemsLocationMap -> Inventory -> World
showInventory paths locations itemsmap items = do
  let my_location = getPos "me" locations
  let current_room = addSpace (getValue my_location paths)
  if items == [] then (paths, locations, itemsmap, items, current_room ++ ": Your Inventory is empty!")
    else do
      let itemsString = "[" ++ intercalate " " items ++ "]"
      (paths, locations, itemsmap, items, "Inventory: " ++ itemsString)
    where
      item = getValue (getPos "me" locations) itemsmap

pickItem :: PathMap -> LocationMap -> ItemsLocationMap -> Inventory -> World
pickItem paths locations itemsmap items = do
  let my_location = getPos "me" locations -- return current Pos
  let item = getValue my_location itemsmap
  let current_room = addSpace (getValue my_location paths)
  if item == "Not Found" || item == "enemy" then (paths, locations, itemsmap, items, current_room ++ ": Nothing to Pick Up!")
  else do
    let new_items = item : items
    let response = "You take the " ++ item
    let new_itemsmap = delete (my_location, item) itemsmap
    (paths, locations, new_itemsmap, new_items, response)

killEnemy :: PathMap -> LocationMap -> ItemsLocationMap -> Inventory -> World
killEnemy paths locations itemsmap items = do
  let my_location = getPos "me" locations
  let enemy = getValue my_location itemsmap
  if enemy == "enemy" then
    if elem "sword" items then do
      let new_itemsmap = delete (my_location, enemy) itemsmap
      (paths, locations, new_itemsmap, items, "You killed the enemy with your sword!")
    else (paths, locations, itemsmap, items, "You died! Game Over!")
  else (paths, locations, itemsmap, items, "No Enemy!")

go :: Move -> PathMap -> LocationMap -> ItemsLocationMap -> Inventory -> World
go direction paths locations itemsmap items = do
  let my_location = getPos "me" locations
  let new_location = move direction my_location
  let new_locations = put "me" new_location locations
  let response = describe new_location paths new_locations
  (paths, new_locations, itemsmap, items, response)

describe :: Pos -> PathMap -> LocationMap -> String
describe new_location paths locations =
  let here = getPos "me" locations
  in description here paths


description :: Pos -> PathMap -> String
description here paths
  | room == "room1" = "Room 1: You in the room 1"
  | room == "room2" = "Room 2: You in the room 2"
  | room == "room3" = "Room 3: You in the room 3"
  | room == "room4" = "Room 4: You in the room 4"
  | room == "room5" = "Room 5: You in the room 5"
  | room == "room6" = "Room 6: You in the room 6"
  | otherwise = "Hi"
  where room = getValue here paths









--
