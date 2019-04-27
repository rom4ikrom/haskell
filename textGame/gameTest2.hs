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
import Inventory

player = Player 100 50 (1,1,0) emptyInventory fists

worldMap = generateMap

type Response = String

type World = (Map, Player, Response)
world :: IO (Map, Player, Response)
world = do
  return (worldMap, player, "")

main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!\n"
          putStrLn gameRules
          putStrLn mapDirections
          putStrLn instructions
          putStrLn room1
          play (return (worldMap, player, ""))
          return "Game Over!"

play :: IO (World) -> IO (World)
play world = do
  (worldMap, player, response) <- world
  putStrLn response
  if (health player) <= 0 then return (worldMap, player, "Quitting.")
  else if response == "Congratulations! You Win!" then return (worldMap, player, "Quitting.")
  else do
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
do_command "eat" gameMap player = eatFood gameMap player
do_command _ gameMap player = (gameMap, player, "Invalid Input!")

go :: Move -> Map -> Player -> World
go direction gameMap player = do
  let new_player = updatePosition player direction
  let current_room = getCurrentFloor (currentPosition new_player) ++ " " ++ addSpace (getRoom (currentPosition new_player) (pathsMap gameMap))
  if (currentPosition new_player) == (2,2,1) && elem "Diamond" (otherItems (currentInventory new_player)) then
    (gameMap, new_player, "Congratulations! You Win!")
  else if player == new_player then (gameMap, new_player, current_room ++ ": No door.")
  else do
    let response = describe (currentPosition new_player) gameMap
    if (energy player) == 0 then (gameMap, reduceHealth new_player 1, response)
    else do
      (gameMap, reduceEnergy new_player 1, response)

pickItem :: Map -> Player -> World
pickItem gameMap player = do
  let my_location = currentPosition player
  let item = getFood my_location (foodMap gameMap)
  let current_room = getCurrentFloor my_location ++ " " ++ addSpace (getRoom my_location (pathsMap gameMap))
  if item == noneFood then (gameMap, player, current_room ++ ": Nothing to Pick Up!")
  else do
    let new_inventory = updateFoodInventory (currentInventory player) (item : (foodItems (currentInventory player)))
    let new_player = updateInventory player new_inventory
    let new_food_map = delete (my_location, item) (foodMap gameMap)
    let response = "You take the " ++ (foodName item) ++ "."
    (updateFoodMap gameMap new_food_map, new_player, response)

changeWeapon :: Map -> Player -> World
changeWeapon gameMap player = do
  let my_location = currentPosition player
  let current_weapon = currentWeapon player
  let weapon = getWeapon my_location (weaponMap gameMap)
  let current_room = getCurrentFloor my_location ++ " " ++ addSpace (getRoom my_location (pathsMap gameMap))
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
  let response = "Health: " ++ show (health player) ++ "\tEnergy: " ++ show (energy player) ++ "\nCurrent Weapon: " ++ (weaponName (currentWeapon player)) ++ "\nFood Inventory: " ++ show (getFoodNames (currentInventory player)) ++ "\nItems Inventory: " ++ show (otherItems (currentInventory player))
  (gameMap, player, response)

killEnemy :: Map -> Player -> World
killEnemy gameMap player = do
  let my_location = currentPosition player
  let weapon = currentWeapon player
  let enemy = getEnemy my_location (enemyMap gameMap)
  let current_room = getCurrentFloor my_location ++ " " ++ addSpace (getRoom my_location (pathsMap gameMap))
  if enemy == noneEnemy then (gameMap, player, current_room ++ ": No Enemy!")
  else do
    let result = kill (energy player) (health player) (enemyHealth enemy) (weaponAttack weapon) (enemyAttack enemy)
    if result  == (0, 0)
      then (gameMap, setHealth player 0, "Game Over!")
    else do
      let new_player = setHealth player (fst result)
      let new_enemy_map = delete (my_location, enemy) (enemyMap gameMap)
      let final_player = setEnergy new_player (snd result)
      if length new_enemy_map == 0 then do
        let new_inventory = updateOtherItemsInventory (currentInventory player) ("Diamond" : (otherItems (currentInventory player)))
        (updateEnemyMap gameMap new_enemy_map, updateInventory final_player new_inventory, "You killed the " ++ (enemyName enemy) ++ " And picked up the secret item!")
      else do
        (updateEnemyMap gameMap new_enemy_map, final_player, "You killed the " ++ (enemyName enemy) ++ "!")

kill :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
kill penergy phealth ehealth pattack eattack | phealth <= 0 = (0, 0)
                                             | ehealth <= 0 = (phealth, penergy)
                                             | penergy <= 0 = kill penergy (phealth - eattack - 2) (ehealth - pattack) pattack eattack
                                             | otherwise = kill (penergy - 2) (phealth - eattack) (ehealth - pattack) pattack eattack

eatFood :: Map -> Player -> World
eatFood gameMap player = do
  let food_items = foodItems (currentInventory player)
  if length food_items == 0 then (gameMap, player, "You have no food!")
  else do
    let food_item = head food_items
    let new_food_items = updateFoodInventory (currentInventory player) (delete food_item food_items)
    let new_health = (health player) + (healthPoints food_item)
    let new_energy = (energy player) + (energyPoints food_item)
    if new_health > 100 then do
      let updated_health_player = setHealth player 100
      let new_player = setEnergy updated_health_player new_energy
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else if new_energy > 50 then do
      let updated_energy_player = setEnergy player 50
      let new_player = setHealth updated_energy_player new_health
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else if new_health > 100 && new_energy > 50 then do
      let updated_health_player = setHealth player 100
      let updated_energy_player = setEnergy updated_health_player 50
      let new_player = updateInventory updated_energy_player new_food_items
      (gameMap, new_player, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else do
      let new_health_player = setHealth player new_health
      let new_player = setEnergy new_health_player new_energy
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show new_health ++ ", Energy: " ++ show new_energy)

describe :: Pos -> Map -> String
describe position gameMap = loadDescription room
  where room = getRoom position (pathsMap gameMap)

getFoodNames :: Inventory -> [String]
getFoodNames items = [foodName x | x <- (foodItems items)]

getFoodItem :: String -> [Food] -> Food
getFoodItem name list = head [x | x <- list, name == foodName x]

getCurrentFloor :: Pos -> String
getCurrentFloor (_,_, -1) = "Basement"
getCurrentFloor (_,_, 0) = "Ground Floor"
getCurrentFloor (_,_, 1) = "First Floor"
getCurrentFloor (_,_,_) = "Not Found!"





















--
