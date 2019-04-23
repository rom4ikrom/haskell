import System.IO
import Data.List
import Data.Char

type Location = String
type Direction = String
type Thing = String
type Response = String

type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
  (("room1", "n"), "room2"),
  (("room2", "s"), "room1"),
  (("room1", "c"), "room1"),
  (("room2", "c"), "room2")
  ]

type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations = [
  ("myself", "room1")
  ]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")

main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!"
          putStrLn instructions
          play (return (paths, locations, ""))
          return "Bye!"

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go in that direction.\n" ++
    "quit               -- to end the game and quit."

play :: IO (World) -> IO (World)
play world = do
  (paths, locations, response) <- world
  putStrLn response
  putStrLn ""
  do
    putStr "command> "
    command <- getLine
    if command == "quit"
      then return (paths, locations, "Quitting.")
      else play (return (do_command command paths locations))

move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

do_command :: String -> PathMap -> LocationMap -> World
do_command "n" paths locations = go "n" paths locations
do_command "e" paths locations = go "e" paths locations
do_command "s" paths locations = go "s" paths locations
do_command "w" paths locations = go "w" paths locations
do_command "u" paths locations = go "u" paths locations
do_command _ paths locations = go "c" paths locations

go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
            let my_location = get "myself" locations
            let new_location = move my_location direction paths
            let new_locations = put "myself" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
  let here = get "myself" locations
  in describe_helper here locations

describe_helper :: Location -> LocationMap -> String
describe_helper here locations = description here

description :: Location -> String
description "room1" = "You in the room 1"
description "room2" = "You in the room 2"
