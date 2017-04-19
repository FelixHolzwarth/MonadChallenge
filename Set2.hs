{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude


data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a)  = "Just " ++ show a
  show Nothing = "Nothing"

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay []     = Nothing

tailMay :: [a] -> Maybe [a]
tailMay (_:xs) = Just xs
tailMay []     = Nothing
