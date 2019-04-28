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

-- creates the player
player = Player 100 50 (1,1,0) emptyInventory fists

-- generates the map
worldMap = generateMap

type Response = String

-- create the world with map, player and responses to user
type World = (Map, Player, Response)
world :: IO (Map, Player, Response)
world = do
  return (worldMap, player, "")

-- prints out the game map, rules and commands
main :: IO (String)
main = do putStrLn "Welcome to the Adventure Game!\n"
          putStrLn gameRules
          putStrLn mapDirections
          putStrLn instructions
          putStrLn room1
          play (return (worldMap, player, ""))
          return "Game Over!"

-- the game is on until user types quit or until win or lose conditions
play :: IO (World) -> IO (World)
play world = do
  (worldMap, player, response) <- world
  putStrLn response
  if (health player) <= 0 then return (worldMap, player, "Quitting.")
  else if response == "Congratulations, you escaped the castle! You Win!" then return (worldMap, player, "Quitting.")
  else do
    putStr "command> "
    command <- getLine
    if command == "quit" then return (worldMap, player, "Quitting.")
    else play (return (do_command command worldMap player))

-- gets food item from food map based on position
getFood :: Eq a => a -> [(a, Food)] -> Food
getFood value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneFood

-- gets the weapon from weapon map based on position
getWeapon :: Eq a => a -> [(a, Weapon)] -> Weapon
getWeapon value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneWeapon

-- gets the enemy from weapon map based on position
getEnemy :: Eq a => a -> [(a, Enemy)] -> Enemy
getEnemy value list = case lookup value list of
                     Just result -> result
                     Nothing -> noneEnemy

-- gets the room from paths map based on position
getValue :: Eq a => a -> [(a, String)] -> String
getValue value list = case lookup value list of
                     Just result -> result
                     Nothing -> "null"

-- does the necessary function accordingly to user input
-- if there is no such command - returns Invalid Input!
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
do_command "use" gameMap player = useItem gameMap player
do_command _ gameMap player = (gameMap, player, "Invalid Input!")

-- changes the current position of the player
go :: Move -> Map -> Player -> World
go direction gameMap player = do
  let new_player = updatePosition player direction
  if player == new_player then (gameMap, new_player, "No door.")
  else do
    let response = describe (currentPosition new_player) gameMap
    if (energy player) == 0 then (gameMap, reduceHealth new_player 1, response)
    else do
      (gameMap, reduceEnergy new_player 1, response)

-- pick the item (food item), add the item to food inventory, delete the item from food map list
pickItem :: Map -> Player -> World
pickItem gameMap player = do
  let my_location = currentPosition player
  let item = getFood my_location (foodMap gameMap)
  if item == noneFood then (gameMap, player, "Nothing to Pick Up!")
  else do
    let new_inventory = updateFoodInventory (currentInventory player) (item : (foodItems (currentInventory player)))
    let new_player = updateInventory player new_inventory
    let new_food_map = delete (my_location, item) (foodMap gameMap)
    let response = "You take the " ++ (foodName item) ++ "."
    let changed_room = (getValue my_location (pathsMap gameMap)) ++ "Changed"
    let new_room_map = map (\p@(f, _) -> if f == my_location then (my_location, changed_room) else p) (pathsMap gameMap)
    let new_map = updatePathsMap gameMap new_room_map
    (updateFoodMap new_map new_food_map, new_player, response)

-- change the weapon if the player's current weapon is weaker, delete the weapon from weapon map
changeWeapon :: Map -> Player -> World
changeWeapon gameMap player = do
  let my_location = currentPosition player
  let current_weapon = currentWeapon player
  let weapon = getWeapon my_location (weaponMap gameMap)
  if weapon == noneWeapon then (gameMap, player, "No Weapon!")
  else do
    if (weaponAttack current_weapon) > (weaponAttack weapon) then (gameMap, player, "You Current Weapon is Stronger!")
    else do
      let new_player = updateWeapon player weapon
      let new_weapons_map = delete (my_location, weapon) (weaponMap gameMap)
      let changed_room = (getValue my_location (pathsMap gameMap)) ++ "Changed"
      let new_room_map = map (\p@(f, _) -> if f == my_location then (my_location, changed_room) else p) (pathsMap gameMap)
      let new_map = updatePathsMap gameMap new_room_map
      let response = "You change weapon to: " ++ (weaponName weapon) ++ "."
      (updateWeaponMap new_map new_weapons_map, new_player, response)

-- prints out the player's health, energy, inventories for food and items
printPlayerInfo :: Map -> Player -> World
printPlayerInfo gameMap player = do
  let response = "Health: " ++ show (health player) ++ "\tEnergy: " ++ show (energy player) ++ "\nCurrent Weapon: " ++ (weaponName (currentWeapon player)) ++ "\nFood Inventory: " ++ show (getFoodNames (currentInventory player)) ++ "\nItems Inventory: " ++ show (otherItems (currentInventory player))
  (gameMap, player, response)

-- kill the enemy, if the player is died - game over, if all enemies were defeated - get the special item key to item inventory
killEnemy :: Map -> Player -> World
killEnemy gameMap player = do
  let my_location = currentPosition player
  let weapon = currentWeapon player
  let enemy = getEnemy my_location (enemyMap gameMap)
  if enemy == noneEnemy then (gameMap, player, "No Enemy!")
  else do
    let result = fight (energy player) (health player) (enemyHealth enemy) (weaponAttack weapon) (enemyAttack enemy)
    if result  == (0, 0)
      then (gameMap, setHealthEnergy player 0 0, "Game Over!")
    else do
      let new_player = setHealthEnergy player (fst result) (snd result)
      let new_enemy_map = delete (my_location, enemy) (enemyMap gameMap)
      let changed_room = (getValue my_location (pathsMap gameMap)) ++ "Changed"
      let new_room_map = map (\p@(f, _) -> if f == my_location then (my_location, changed_room) else p) (pathsMap gameMap)
      let new_map = updatePathsMap gameMap new_room_map
      if length new_enemy_map == 0 then do
        let new_inventory = updateOtherItemsInventory (currentInventory player) ("Key" : (otherItems (currentInventory player)))
        (updateEnemyMap new_map new_enemy_map, updateInventory new_player new_inventory, "You killed the " ++ (enemyName enemy) ++ " and picked up the key!")
      else do
        (updateEnemyMap new_map new_enemy_map, new_player, "You killed the " ++ (enemyName enemy) ++ "!")

-- determines who win: if enemy - return (0,0)
-- if player -- return the remainder of (health, energy)
-- if during the fight energy becomes 0, the health will reduce by 2 points more each hit
fight :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
fight penergy phealth ehealth pattack eattack | phealth <= 0 = (0, 0)
                                              | ehealth <= 0 = (phealth, penergy)
                                              | penergy <= 0 = fight penergy (phealth - eattack - 2) (ehealth - pattack) pattack eattack
                                              | otherwise = fight (penergy - 4) (phealth - eattack) (ehealth - pattack) pattack eattack
-- eat the first food item from food iventory
-- if the inventory has no items return You have no food!
-- otherwise increase the health and energy by food specifications,
-- but the health cannot be more than 100 and enegy - more than 50
eatFood :: Map -> Player -> World
eatFood gameMap player = do
  let food_items = foodItems (currentInventory player)
  if length food_items == 0 then (gameMap, player, "You have no food!")
  else do
    let food_item = head food_items
    let new_food_items = updateFoodInventory (currentInventory player) (delete food_item food_items)
    let new_health = (health player) + (healthPoints food_item)
    let new_energy = (energy player) + (energyPoints food_item)
    if new_health >= 100 && new_energy < 50 then do
      let new_player = setHealthEnergy player 100 new_energy
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else if new_energy >= 50 && new_health < 100 then do
      let new_player = setHealthEnergy player new_health 50
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else if new_health >= 100 && new_energy >= 50 then do
      let updated_player = setHealthEnergy player 100 50
      let new_player = updateInventory updated_player new_food_items
      (gameMap, new_player, "You have Health: " ++ show (health new_player) ++ ", Energy: " ++ show (energy new_player))
    else do
      let new_player = setHealthEnergy player new_health new_energy
      (gameMap, updateInventory new_player new_food_items, "You have Health: " ++ show new_health ++ ", Energy: " ++ show new_energy)

useItem :: Map -> Player -> World
useItem gameMap player = do
  let my_location = currentPosition player
  let items_inventory = (otherItems (currentInventory player))
  if elem "Key" items_inventory then do
    let chest = getValue my_location (itemsMap gameMap)
    if chest == "null" then (gameMap, player, "No chest to open!")
    else do
      let new_items_inventory = delete "Key" items_inventory
      let new_inventory = updateOtherItemsInventory (currentInventory player) ("Teleport" : new_items_inventory)
      let new_items_map = delete (my_location, chest) (itemsMap gameMap)
      (updateItemsMap gameMap new_items_map, updateInventory player new_inventory, "You found the Teleport in the chest! Now you need to find the special room where you can use the teleport!")
  else if elem "Teleport" items_inventory then do
    if (currentPosition player) == (2,2,1) && elem "Teleport" (otherItems (currentInventory player)) then
      (gameMap, player, "Congratulations, you escaped the castle! You Win!")
    else (gameMap, player, "This is the wrong place to use the Teleport!")
  else (gameMap, player, "You have nothing to use!")

-- loads the room description
describe :: Pos -> Map -> String
describe position gameMap = loadDescription room
  where room = getValue position (pathsMap gameMap)

-- shows the food inventory to the user
getFoodNames :: Inventory -> [String]
getFoodNames items = [foodName x | x <- (foodItems items)]

-- shows the item inventory to the user
getFoodItem :: String -> [Food] -> Food
getFoodItem name list = head [x | x <- list, name == foodName x]

-- gets the current floor
getCurrentFloor :: Pos -> String
getCurrentFloor (_,_, -1) = "Basement"
getCurrentFloor (_,_, 0) = "Ground Floor"
getCurrentFloor (_,_, 1) = "First Floor"
getCurrentFloor (_,_,_) = "Not Found!"





















--
