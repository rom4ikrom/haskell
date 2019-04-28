module RoomsDescription where

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go North South East West Up Down.\n" ++
    "pick               -- to pick up the food.\n" ++
    "change             -- to pick up and change the current weapon.\n" ++
    "kill               -- attack the enemy.\n" ++
    "info               -- to see your health, energy, inventory and current weapon.\n" ++
    "eat                -- to eat food from your inventory to increase health and energy.\n" ++
    "use                -- to use the item from you item inventory.\n" ++
    "quit               -- to end the game and quit.\n"

mapDirections =
  "Game Map\n" ++
  "Basement         Ground Floor      First Floor\n" ++
  "-------------    -------------     -------------\n" ++
  "| 4 ! 5 ! 6 |    | 4 ! 5 ! 6 |     | 4 ! 5 ! 6 |\n" ++
  "-   -   -   -    -   -   -   -     -   -   -   -\n" ++
  "| 1 ! 2 ! 3 |    | 1 ! 2 ! 3 |     | 1 ! 2 ! 3 |\n" ++
  "-------------    -------------     -------------\n" ++
  "3UP              2UP 6UP 3DOWN     2DOWN 6DOWN\n"

gameRules =
  "Rules: At the start you have Health 100, Energy 50 and Weapon Fists. Each move will subtract one energy.\n" ++
  "When the energy would be 0, you will lose 1 health point per move. During the fight you will lose 2 energy per 1 hit.\n" ++
  "Win Condition: 1. Kill all enemies; 2. Enter the one special room and use the special item! Good Luck!\n"

-- description for each room
room1 = "Ground Floor Room 1: You are in room 1. This is an empty dark room. You can hear\n" ++
        "scary noises from the door in front of you. There is a door to your right."

room2 = "Ground Floor Room 2: You are in room 2. This is a light room with stairs up. You can hear that\n" ++
        "somebody is walking in the room in front of you. There is a door to your right\n" ++
        "to enter the Room 3. You can use the door to your left to enter the Room 1."

room3 = "Ground Floor Room 3: You see a fresh apple on the floor. It looks very nice. In front\n" ++
        "of you can see a slightly opened door. To your left is a door to Room 2.\n" ++
        "Moreover, you see the stairs to basement!"

room4 = "Ground Floor Room 4: Now you know the reason of scary noises when you was in Room 1. There is\n" ++
        "a big zombie in front of you. You can not beat him without any weapon. There are two ways\n" ++
        "to run: back - Room 1, right - Room 5."

room5 = "Ground Floor Room 5: There is a small goblin. You can think that he is not harmful but it's not.\n" ++
        "He can easily kill you. You can escape using door to you right or left or back."

room6 = "Ground Floor Room 6: You can see a big sword on the floor. With this weapon, you can easily beat\n" ++
        "all enemies on your way. Pick up the sword and go kill your first enemy. Hint: use door\n" ++
        "to your left. Also you can go back to enter Room 3 or up."

room1A = "Floor 1 Room 1A: You found the most powerfull weapon - pistol! It will help you to escape this building!\n" ++
         "To the north you can see the opened door from where comes the awful smell. Should be someone not friendly\n" ++
         "there."

room2A = "Floor 1 Room 2A: This is a small dark room with stairs to the ground floor. You can feel some magic is happening\n" ++
         "in the room in front of you. You can smell that the chocolate is somewhere near you!"

room3A = "Floor 1 Room 3A: You found the chocolate bar on the table. Pick up it and eat to restore health and energy!"

room4A = "Floor 1 Room 4A: There is a big dangerous monster!!! Run away or fight like a hero!"

room5A = "Floor 1 Room 5A: This room seems to very very strange... but it can help you to achieve you want..."

room6A = "Floor 1 Room 6A: Light room with stairs to the ground floor. You can go down, west and south!"

room1B = "Basement Room 1B: There is a goblin with magic powers! Be careful..."

room2B = "Basement Room 2B: You found a big tasty steak on the plate. It will restore a great amount of health and energy.\n" ++
         "Eat it when you have very low health."

room3B = "Basement Room 3B: This is a small room with stairs to the ground floor. To the north there is a pantry room.\n" ++
         "To the west from you there is a kitchen. You may found something in both of these rooms."

room4B = "Basement Room 4B: Nothing to do in this room. But you can hear that someone is talking in the south room."

room5B = "Basement Room 5B: You found a chest. To open it you need to find the key. On the wall you read: FIGHT TO THE END\n" ++
         "What could it mean? Hmmm...."

room6B = "Basement Room 6B: This is a pantry room. The smell is awfull. On the floor, there is a bottle of spoiled milk.\n" ++
         "It can harm yourself."

-- description for rooms when the item was taken from the room or enemy was killed
here = "You have been here."

--room3 after the apple was taken
room3Changed = "Ground Floor Room 3: " ++ here ++ " You already took the apple before! You can go down to the basement!"
--room4 after the enemy was killed
room4Changed = "Ground Floor Room 4: " ++ here ++ " You already killed the enemy!"
--room5 after the enemy was killed
room5Changed = "Ground Floor Room 5: " ++ here ++ " You already killed the enemy!"
--room6 after the sword was taken
room6Changed = "Ground Floor Room 6: " ++ here ++ " You already took the sword! You can go up to the first floor!"

--room1A after the pistol was taken
room1AChanged = "Floor 1 Room 1A: " ++ here ++ " You already took the pistol!"
--room3A after the chocolate was taken
room3AChanged = "Floor 1 Room 3A: " ++ here ++ " You already took the chocolate!"
--room4A after the enemy was killed
room4AChanged = "Floor 1 Room 4A: " ++ here ++ " You already killed the enemy!"

--room1B after the enemy was killed
room1BChanged = "Basement Room 1B " ++ here ++ " You already killed the enemy!"
--room2B after the steak was taken
room2BChanged = "Basement Room 2B " ++ here ++ " You already took the steak!"
--room6B after the spoiled mild was taken
room6BChanged = "Basement Room 6B " ++ here ++ " You already took the spoiled milk!"

loadDescription :: String -> String
loadDescription xs | xs == "Room 1" = room1
                   | xs == "Room 2" = room2
                   | xs == "Room 3" = room3
                   | xs == "Room 4" = room4
                   | xs == "Room 5" = room5
                   | xs == "Room 6" = room6
                   | xs == "Room 1A" = room1A
                   | xs == "Room 2A" = room2A
                   | xs == "Room 3A" = room3A
                   | xs == "Room 4A" = room4A
                   | xs == "Room 5A" = room5A
                   | xs == "Room 6A" = room6A
                   | xs == "Room 1B" = room1B
                   | xs == "Room 2B" = room2B
                   | xs == "Room 3B" = room3B
                   | xs == "Room 4B" = room4B
                   | xs == "Room 5B" = room5B
                   | xs == "Room 6B" = room6B
                   | xs == "Room 3Changed" = room3Changed
                   | xs == "Room 4Changed" = room4Changed
                   | xs == "Room 5Changed" = room5Changed
                   | xs == "Room 6Changed" = room6Changed
                   | xs == "Room 1AChanged" = room1AChanged
                   | xs == "Room 3AChanged" = room3AChanged
                   | xs == "Room 4AChanged" = room4AChanged
                   | xs == "Room 1BChanged" = room1BChanged
                   | xs == "Room 2BChanged" = room2BChanged
                   | xs == "Room 6BChanged" = room6BChanged
                   | otherwise = "You Lost!"
