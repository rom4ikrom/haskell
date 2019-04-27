import Data.List
import System.IO
import Data.Char

import Move
import Map
import Player
import RoomsDescription

player = Player 100 50 (1,1,0) []

worldMap = generateMap

type Response = String

type World = (Map, Player, Response)
world :: IO (Map, Player, Response)
world = do
  return (worldMap, player, "")

main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!"
          putStrLn ""
          putStrLn instructions
          putStrLn room1
          play (return (worldMap, player, ""))
          return "Bye!"

play :: IO (World) -> IO (World)
play world = do
  (worldMap, player, response) <- world
  putStrLn response
  if response == "You died! Game Over!" then return (worldMap, player, "Quitting.")
  else do
    putStr "command> "
    command <- getLine
    if command == "quit" then return (worldMap, player, "Quitting.")
    else play (return (do_command command worldMap player))

getValue :: Eq a => a -> [(a, String)] -> String
getValue value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not Found"

addSpace :: String -> String
addSpace xs = if length xs <= 4
              then upperFirstLetter xs
              else upperFirstLetter (take 4 xs ++ " " ++ addSpace (drop 4 xs))

upperFirstLetter :: String -> String
upperFirstLetter (x:xs) = toUpper x : xs

do_command :: String -> Map -> Player -> World
do_command "n" gameMap player = go North  gameMap player
do_command "s" gameMap player = go South  gameMap player
do_command "w" gameMap player = go West   gameMap player
do_command "e" gameMap player = go East   gameMap player
do_command "u" gameMap player = go Up     gameMap player
do_command "d" gameMap player = go Down   gameMap player
do_command "p" gameMap player = pickItem gameMap player
do_command "h" gameMap player = printPlayerInfo gameMap player
do_command _ gameMap player = (gameMap, player, "Invalid Input!")

go :: Move -> Map -> Player -> World
go direction gameMap player = do
  let new_player = updatePosition player direction
  let current_room = addSpace (getValue (currentPosition player) (pathsMap gameMap))
  if player == new_player then (gameMap, new_player, current_room ++ ": No door.")
  else do
    let response = describe (currentPosition new_player) gameMap
    if (energy player) == 0 then (gameMap, reduceHealth new_player 1, response)
    else do
      (gameMap, reduceEnergy new_player, response)

pickItem :: Map -> Player -> World
pickItem gameMap player = do
  let my_location = currentPosition player
  let item = getValue my_location (itemsMap gameMap)
  let current_room = addSpace (getValue my_location (pathsMap gameMap))
  if item == "Not Found" || item == "enemy" then (gameMap, player, current_room ++ ": Nothing to Pick Up!")
  else do
    let new_inventory = item : (currentInventory player)
    let new_player = updateInventory player new_inventory
    let new_items_map = delete (my_location, item) (itemsMap gameMap)
    let response = "You take the " ++ item
    (updateItemsMap gameMap new_items_map, new_player, response)

printPlayerInfo :: Map -> Player -> World
printPlayerInfo gameMap player = do
  let response = "Health: " ++ show (health player) ++ "\tEnergy: " ++ show (energy player) ++"\nInventory: "++show (currentInventory player)
  (gameMap, player, response)

describe :: Pos -> Map -> String
describe position gameMap = loadDescription room
  where room = getValue position (pathsMap gameMap)





















--
