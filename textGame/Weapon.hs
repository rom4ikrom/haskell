module Weapon
( Weapon(..)
, fists
, sword
, pistol
, noneWeapon
) where

data Weapon = Weapon { weaponName :: String
                     , weaponAttack :: Int
} deriving (Show, Eq)

fists = Weapon "Fists" 7

sword = Weapon "Sword" 30

pistol = Weapon "Pistol" 45

noneWeapon = Weapon "" 0
