module Move
(
  Pos(..)
  ,Move(..)
  ,move
  ,checkMove
  ,correctRoomUp
  ,correctRoomDown
) where

-- Pos (x, y, z)
-- x - x coodinate
-- y - y coordinate
-- z - floor -1 basement; 0 ground floor; 1 first floor
type Pos = (Int, Int, Int)

data Move = North | South | West | East | Up | Down

-- change the position accordingly to user move (input)
move :: Move -> Pos -> Pos
move North (x, y, z) = checkMove (x, y + 1, z)
move South (x, y, z) = checkMove (x, y - 1, z)
move West (x, y, z) = checkMove (x - 1, y, z)
move East (x, y, z) = checkMove (x + 1, y, z)
move Up (x, y, z) = correctRoomUp (x, y, z)
move Down (x, y, z) = correctRoomDown (x, y, z)

-- checks if the move is inside the map (for example from room1 you can north and east, but not south and west)
checkMove :: Pos -> Pos
checkMove (x, y, z) = do
  if x == 4 then (x - 1, y, z)
  else if x == 0 then (x + 1, y, z)
  else if y == 3 then (x, y - 1, z)
  else if y == 0 then (x, y + 1, z)
  else if z == 2 then (x, y, z - 1)
  else if z == -2 then (x, y, z + 1)
  else (x, y, z)

-- the player can go up only from specified room
-- from ground floor to first floor - from room2 and room6
-- from basement to the ground floor - only from room3
correctRoomUp :: Pos -> Pos
correctRoomUp (x, y, z) = do
  if x == 2 && y == 1 && z == 0 then (x, y, z + 1)
  else if x == 3 && y == 2 && z == 0 then (x, y, z + 1)
  else if x == 3 && y == 1 && z == -1 then (x, y, z + 1)
  else (x, y, z)

-- the player can go down only from specified room
-- from first floor to ground floor - from room2 and room6
-- from ground floor to the basement - only from room3
correctRoomDown :: Pos -> Pos
correctRoomDown (x, y, z) = do
  if x == 2 && y == 1 && z == 1 then (x, y, z - 1)
  else if x == 3 && y == 2 && z == 1 then (x, y, z - 1)
  else if x == 3 && y == 1 && z == 0 then (x, y, z - 1)
  else (x, y, z)
