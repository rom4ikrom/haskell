module Enemy
( Enemy(..)
, zombie
, goblin
, noneEnemy
) where

data Enemy = Enemy { enemyName :: String
                   , enemyHealth :: Int
                   , enemyAttack :: Int
} deriving (Show, Eq)

zombie = Enemy "Zombie" 70 25
goblin = Enemy "Goblin" 50 12

noneEnemy = Enemy "" 0 0
