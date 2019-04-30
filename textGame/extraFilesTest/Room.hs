module Room
( Room(..)

) where

type Objects = [(Int, String)]

data Room = Room { roomName :: String
                 , closed :: Bool
                 , objects :: Objects
                 , description :: String
} deriving (Show, Eq)

--rooms
room1 = "Room 1" False room1objects room1info
room2 = "Room 2" False [] room2info

-- rooms objests
room1objects = [(0, "Book"), (1, "Chest")]

-- rooms description
room1info = ""
room2info = ""
