import Data.List
import System.IO
import Data.Char

import Move
import Map
import Player
import Weapon
import Food
import Enemy
import RoomsDescription


player = Player 100 50 (1,1,0) [] fists

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
          return "You Died! Game Over!"

play :: IO (World) -> IO (World)
play world = do
  (worldMap, player, response) <- world
  if (health player) <= 0 then return (worldMap, player, "Quitting.")
  else do
    putStrLn response
    putStr "command> "
    command <- getLine
    if command == "quit" then return (worldMap, player, "Quitting.")
    else play (return (do_command command worldMap player))

getFood :: Eq a => a -> [(a, Food)] -> Food
getFood value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneFood

getWeapon :: Eq a => a -> [(a, Weapon)] -> Weapon
getWeapon value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneWeapon

getEnemy :: Eq a => a -> [(a, Enemy)] -> Enemy
getEnemy value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneEnemy

getRoom :: Eq a => a -> [(a, String)] -> String
getRoom value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not Found."

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
do_command "pick" gameMap player = pickItem gameMap player
do_command "change" gameMap player = changeWeapon gameMap player
do_command "info" gameMap player = printPlayerInfo gameMap player
do_command "kill" gameMap player = killEnemy gameMap player
do_command _ gameMap player = (gameMap, player, "Invalid Input!")

go :: Move -> Map -> Player -> World
go direction gameMap player = do
  let new_player = updatePosition player direction
  let current_room = addSpace (getRoom (currentPosition player) (pathsMap gameMap))
  if player == new_player then (gameMap, new_player, current_room ++ ": No door.")
  else do
    let response = describe (currentPosition new_player) gameMap
    if (energy player) == 0 then (gameMap, reduceHealth new_player 1, response)
    else do
      (gameMap, reduceEnergy new_player 1, response)

pickItem :: Map -> Player -> World
pickItem gameMap player = do
  let my_location = currentPosition player
  let item = getFood my_location (foodMap gameMap)
  let current_room = addSpace (getRoom my_location (pathsMap gameMap))
  if item == noneFood then (gameMap, player, current_room ++ ": Nothing to Pick Up!")
  else do
    let new_inventory = item : (currentInventory player)
    let new_player = updateInventory player new_inventory
    let new_food_map = delete (my_location, item) (foodMap gameMap)
    let response = "You take the " ++ (foodName item) ++ "."
    (updateFoodMap gameMap new_food_map, new_player, response)

changeWeapon :: Map -> Player -> World
changeWeapon gameMap player = do
  let my_location = currentPosition player
  let current_weapon = currentWeapon player
  let weapon = getWeapon my_location (weaponMap gameMap)
  let current_room = addSpace (getRoom my_location (pathsMap gameMap))
  if weapon == noneWeapon then (gameMap, player, current_room ++ ": No Weapon!")
  else do
    if (weaponAttack current_weapon) > (weaponAttack weapon) then (gameMap, player, current_room ++ ": You Current Weapon is Stronger!")
    else do
      let new_player = updateWeapon player weapon
      let new_weapons_map = delete (my_location, weapon) (weaponMap gameMap)
      let response = "You change weapon to: " ++ (weaponName weapon) ++ "."
      (updateWeaponMap gameMap new_weapons_map, new_player, response)

printPlayerInfo :: Map -> Player -> World
printPlayerInfo gameMap player = do
  let response = "Health: " ++ show (health player) ++ "\tEnergy: " ++ show (energy player) ++ "\nCurrent Weapon: " ++ (weaponName (currentWeapon player)) ++ "\nInventory: " ++ show (getFoodNames (currentInventory player))
  (gameMap, player, response)

killEnemy :: Map -> Player -> World
killEnemy gameMap player = do
  let my_location = currentPosition player
  let weapon = currentWeapon player
  let enemy = getEnemy my_location (enemyMap gameMap)
  let current_room = addSpace (getRoom my_location (pathsMap gameMap))
  if enemy == noneEnemy then (gameMap, player, current_room ++ ": No Enemy!")
  else do
    let result = kill (energy player) (health player) (enemyHealth enemy) (weaponAttack weapon) (enemyAttack enemy)
    if result  == (0, 0)
      then (gameMap, setHealth player 0, "Game Over!")
    else do
      let new_enemy_map = delete (my_location, enemy) (enemyMap gameMap)
      let new_player = setHealth player (fst result)
      (updateEnemyMap gameMap new_enemy_map, setEnergy new_player (snd result), "You killed the " ++ (enemyName enemy) ++ "!")

kill :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
kill penergy phealth ehealth pattack eattack | phealth <= 0 = (0, 0)
                                             | ehealth <= 0 = (phealth, penergy)
                                             | penergy <= 0 = kill penergy (phealth - eattack - 2) (ehealth - pattack) pattack eattack
                                             | otherwise = kill (penergy - 2) (phealth - eattack) (ehealth - pattack) pattack eattack

describe :: Pos -> Map -> String
describe position gameMap = loadDescription room
  where room = getRoom position (pathsMap gameMap)

getFoodNames :: Inventory -> [String]
getFoodNames items = [foodName x | x <- items]





















--
