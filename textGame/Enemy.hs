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

zombie = Enemy "Zombie" 70 22
goblin = Enemy "Goblin" 50 10
monster = Enemy "Monster" 95 28

noneEnemy = Enemy "" 0 0
