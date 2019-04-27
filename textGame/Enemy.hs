module Enemy
( Enemy(..)
, zombie
, goblin
, monster
, noneEnemy
) where

data Enemy = Enemy { enemyName :: String
                   , enemyHealth :: Int
                   , enemyAttack :: Int
} deriving (Show, Eq)

zombie = Enemy "Zombie" 70 25
goblin = Enemy "Goblin" 50 12
monster = Enemy "Monster" 95 30

noneEnemy = Enemy "" 0 0
