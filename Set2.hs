module Set2 where

import MCPrelude()
import Set1()

data Maybe a = Just a | Nothing

instance Show a => Show (Set2.Maybe a) where
  show Just a  = "a"
  show Nothing = "Nothing "
