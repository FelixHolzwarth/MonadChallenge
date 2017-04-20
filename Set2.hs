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

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay elem lst = case filter (\(a,b) -> elem == a) lst of
  [(a,b)]   -> Just b
  [(a,b),_] -> Just b
  otherwise -> Nothing

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just (div a b)
