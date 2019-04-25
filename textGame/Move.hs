module Move where

type Pos = (Int, Int, Int)

data Move = North | South | West | East | Up | Down
move :: Move -> Pos -> Pos
move North (x, y, z) = checkMove (x, y + 1, z)
move South (x, y, z) = checkMove (x, y - 1, z)
move West (x, y, z) = checkMove (x - 1, y, z)
move East (x, y, z) = checkMove (x + 1, y, z)
move Up (x, y, z) = correctRoomUp (x, y, z)
move Down (x, y, z) = correctRoomDown (x, y, z)

checkMove :: Pos -> Pos
checkMove (x, y, z) = do
  if x == 4 then (x - 1, y, z)
  else if x == 0 then (x + 1, y, z)
  else if y == 3 then (x, y - 1, z)
  else if y == 0 then (x, y + 1, z)
  else if z == 2 then (x, y, z - 1)
  else if z == -2 then (x, y, z + 1)
  else (x, y, z)

correctRoomUp :: Pos -> Pos
correctRoomUp (x, y, z) = do
  if x == 2 && y == 1 && z == 0 then (x, y, z + 1)
  else if x == 3 && y == 2 && z == 0 then (x, y, z + 1)
  else if x == 3 && y == 1 && z == -1 then (x, y, z + 1)
  else (x, y, z)

correctRoomDown :: Pos -> Pos
correctRoomDown (x, y, z) = do
  if x == 2 && y == 1 && z == 1 then (x, y, z - 1)
  else if x == 3 && y == 2 && z == 1 then (x, y, z - 1)
  else if x == 3 && y == 1 && z == 0 then (x, y, z - 1)
  else (x, y, z)
