module RoomsDescription where

instructions =
    "Enter commands using one or two words.\n" ++
    "Available commands are:\n" ++
    "main               -- to start the game.\n" ++
    "n  s  e  w  u  d   -- to go North South East West Up Down.\n" ++
    "pick               -- to pick up the item.\n" ++
    "kill               -- attack the enemy.\n" ++
    "i                  -- to see your inventory (what you are holding).\n" ++
    "quit               -- to end the game and quit.\n"

room1 = "Room 1: You are in room 1. This is an empty dark room. You can hear\n" ++
        "scary noises from the door in front of you. There is a door to your right."

room2 = "Room 2: You are in room 2. This is a light room. You can hear that\n" ++
        "somebody is walking in the room in front of you. There is a door to your right\n" ++
        "to enter the Room 3. You can use the door to your left to enter the Room 1."

room3 = "Room 3: You see a fresh apple on the floor. It looks very nice. In front\n" ++
        "of you can see a slightly opened door. To you left is a door to Room 2."

room4 = "Room 4: Now you know the reason of scary noises when you was in Room 1. There is\n" ++
        "a big zombie in front of you. You can not beat him without any weapon. There are two ways\n" ++
        "to run: back - Room 1, right - Room 5."

room5 = "Room 5: There is a small goblin. You can think that he is not harmful but it's not.\n" ++
        "He can easily kill you. You can escape using door to you right or left or back."

room6 = "Room 6: You can see a big sword on the floor. With this weapon, you can easily beat\n" ++
        "all enemies on your way. Pick up the sword and go kill your first enemy. Hint: use door\n" ++
        "to your left. Or go back to enter Room 3."

room1A = "Room 1A: "
room2A = "Room 2A: "
room3A = "Room 3A: "
room4A = "Room 4A: "
room5A = "Room 5A: "
room6A = "Room 6A: "

room1B = "Room 1B: "
room2B = "Room 2B: "
room3B = "Room 3B: "
room4B = "Room 4B: "
room5B = "Room 5B: "
room6B = "Room 6B: "

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
