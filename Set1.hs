
module Set1 where

import MCPrelude


fiveRands :: [Integer]
fiveRands = take 5 (map fst (iterate (rand . snd) (rand $ mkSeed 1)))


randLetter :: Seed -> Gen Char
randLetter seed = (rndchar,newSeed)
  where
    rndTupel = rand seed
    rndchar    = toLetter $ fst rndTupel
    newSeed = snd rndTupel


randString3 :: String
randString3 = take 3 $ map fst $ iterate (randLetter . snd) (randLetter $ mkSeed 1)

type Gen a = (a,Seed)
type Gen' a = Seed -> (a,Seed)

randEven :: Seed -> Gen Integer -- the output of rand * 2
randEven seed = (doubledValue,snd rndTupel)
  where
    rndTupel = rand seed
    doubledValue = (*) 2 $ fst rndTupel

randOdd :: Seed ->  Gen Integer -- the output of rand * 2 + 1
randOdd seed = (modifiedValue,snd rndTupel)
  where
    rndTupel = randEven seed
    modifiedValue = (+) 1 $ fst rndTupel

randTen :: Seed ->  Gen Integer -- the output or rand * 10
randTen seed = (modifiedValue, snd rndTupel)
  where
    rndTupel = rand seed
    modifiedValue = (*) 10 $ fst rndTupel


randA :: (a -> b) -> Gen a -> Gen b
randA f (a,seed) = (f a,seed)

randA' :: (a -> b) -> Gen' a -> Gen' b
randA' f = aplicationFunc f . getTuple
  where
    getTuple = rand
    aplicationFunc f = (\(a,newSeed) -> (f a, newSeed))

randEven' :: Seed -> Gen Integer
randEven' seed = modifiedTupel
  where
    rndTupel = rand seed
    modifiedTupel = randA (\x -> (*) x 2) rndTupel

randOdd' :: Seed -> Gen Integer
randOdd' seed = modifiedTupel
      where
        rndTupel = randEven' seed
        modifiedTupel = randA (\x -> (+) x 1) rndTupel

randTen' :: Seed -> Gen Integer
randTen' seed = modifiedTupel
  where
    rndTupel = rand seed
    modifiedTupel = randA (\x -> (*) x 10) rndTupel

valuesList :: [Integer]
valuesList = map fst [randEven' $ mkSeed 1,randOdd' $ mkSeed 1, randTen' $ mkSeed 1]
