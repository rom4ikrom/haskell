module RoomsDescription where

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go North South East West Up Down.\n" ++
    "pick               -- to pick up the item.\n" ++
    "kill               -- attack the enemy.\n" ++
    "info               -- to see your health, energy, inventory and current weapon.\n" ++
    "eat                -- to eat food from your inventory to increase health and energy.\n" ++
    "quit               -- to end the game and quit.\n"

mapDirections =
  "Game Map\n" ++
  "Basement         Ground Floor      First Floor\n" ++
  "-------------    -------------     -------------\n" ++
  "| 4 | 5 | 6 |    | 4 | 5 | 6 |     | 4 | 5 | 6 |\n" ++
  "-   -   -   -    -   -   -   -     -   -   -   -\n" ++
  "| 1 | 2 | 3 |    | 1 | 2 | 3 |     | 1 | 2 | 3 |\n" ++
  "-------------    -------------     -------------\n"

gameRules =
  "Rules: At the start you have Health 100, Energy 50 and Weapon Fists. Each move will subtract one energy.\n" ++
  "When the energy would be 0, you will lose 1 health point per move. During the fight you will lose 2 energy per 1 hit.\n" ++
  "Win Condition: 1. Kill all enemies; 2. Enter the one special room and use the special item! Good Luck!\n"

room1 = "Ground Floor Room 1: You are in room 1. This is an empty dark room. You can hear\n" ++
        "scary noises from the door in front of you. There is a door to your right."

room2 = "Ground Floor Room 2: You are in room 2. This is a light room with stairs. You can hear that\n" ++
        "somebody is walking in the room in front of you. There is a door to your right\n" ++
        "to enter the Room 3. You can use the door to your left to enter the Room 1 or go up."

room3 = "Ground Floor Room 3: You see a fresh apple on the floor. It looks very nice. In front\n" ++
        "of you can see a slightly opened door. To you left is a door to Room 2.\n" ++
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

room2A = "Floor 1 Room 2A: "
room3A = "Floor 1 Room 3A: "
room4A = "Floor 1 Room 4A: "
room5A = "Floor 1 Room 5A: "
room6A = "Floor 1 Room 6A: "

room1B = "Basement Room 1B: "
room2B = "Basement Room 2B: "
room3B = "Basement Room 3B: "
room4B = "Basement Room 4B: "
room5B = "Basement Room 5B: "
room6B = "Basement Room 6B: "

loadDescription :: String -> String
loadDescription xs | xs == "room1" = room1
                   | xs == "room2" = room2
                   | xs == "room3" = room3
                   | xs == "room4" = room4
                   | xs == "room5" = room5
                   | xs == "room6" = room6
                   | xs == "room1A" = room1A
                   | xs == "room2A" = room2A
                   | xs == "room3A" = room3A
                   | xs == "room4A" = room4A
                   | xs == "room5A" = room5A
                   | xs == "room6A" = room6A
                   | xs == "room1B" = room1B
                   | xs == "room2B" = room2B
                   | xs == "room3B" = room3B
                   | xs == "room4B" = room4B
                   | xs == "room5B" = room5B
                   | xs == "room6B" = room6B
                   | otherwise = "You Lost!"
