module Data.Mod
  ( Mod(..)
  )
where

newtype Mod a = Mod
    { unMod :: a -> a }

instance Semigroup (Mod a) where
  Mod f <> Mod g = Mod (f . g)
