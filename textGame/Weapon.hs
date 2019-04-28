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

fists = Weapon "Fists" 9

sword = Weapon "Sword" 34

pistol = Weapon "Pistol" 48

noneWeapon = Weapon "" 0
