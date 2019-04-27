import Data.List
import System.IO
import Data.Char

import Move
import Player
import RoomsDescription

player = Player 100 50 (1,1,0) []

type Location = String
type Thing = String
type Response = String

type PathMap = [(Pos, Location)]
paths :: PathMap
paths = [
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

type ItemsLocationMap = [(Pos, Thing)]
itemsmap :: ItemsLocationMap
itemsmap = [
  ((3,1,0), "apple"),
  ((3,2,0), "sword"),
  ((2,2,0), "enemy"),
  ((1,2,0), "enemy")
  ]

type World = (PathMap, Player, Response)
world :: IO (PathMap, Player, Response)
world = do
  return (paths, player, "")

main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!"
          putStrLn ""
          putStrLn instructions
          putStrLn room1
          play (return (paths, player, ""))
          return "Bye!"

play :: IO (World) -> IO (World)
play world = do
  (paths, player, response) <- world
  putStrLn response
  if response == "You died! Game Over!" then return (paths, player, "Quitting.")
  else do
    putStr "command> "
    command <- getLine
    if command == "quit" then return (paths, player, "Quitting.")
    else play (return (do_command command paths player))

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

do_command :: String -> PathMap -> Player -> World
do_command "n" paths player = go North  paths player
do_command "s" paths player = go South  paths player
do_command "w" paths player = go West   paths player
do_command "e" paths player = go East   paths player
do_command "u" paths player = go Up     paths player
do_command "d" paths player = go Down   paths player
do_command "p" paths player = pickItem paths player
do_command "h" paths player = printPlayerInfo paths player
do_command _ paths player = (paths, player, "Invalid Input!")

go :: Move -> PathMap -> Player -> World
go direction paths player = do
  let new_player = updatePosition player direction
  let current_room = addSpace (getValue (currentPosition player) paths)
  if player == new_player then (paths, new_player, current_room ++ ": No door.")
  else do
    let response = describe (currentPosition new_player) paths
    if (energy player) == 0 then (paths, reduceHealth new_player 1, response)
    else do
      (paths, reduceEnergy new_player, response)

pickItem :: PathMap -> Player -> World
pickItem paths player = do
  let my_location = currentPosition player
  let item = getValue my_location itemsmap
  let current_room = addSpace (getValue my_location paths)
  if item == "Not Found" || item == "enemy" then (paths, player, current_room ++ ": Nothing to Pick Up!")
  else do
    let new_inventory = item : (currentInventory player)
    let new_player = updateInventory player new_inventory
    let response = "You take the " ++ item
    (paths, new_player, response)

printPlayerInfo :: PathMap -> Player -> World
printPlayerInfo paths player = do
  let response = "Health: " ++ show (health player) ++ "\tEnergy: " ++ show (energy player) ++"\nInventory: "++show (currentInventory player)
  (paths, player, response)

describe :: Pos -> PathMap -> String
describe position paths = loadDescription room
  where room = getValue position paths





















--
