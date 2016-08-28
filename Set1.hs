
module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = take 5 (map fst (iterate (rand . snd) (rand $ mkSeed 1)))


randLetter :: Gen Char
randLetter seed = (rndchar,newSeed)
  where
    rndTupel = rand seed
    rndchar    = toLetter $ fst rndTupel
    newSeed = snd rndTupel


randString3 :: String
randString3 = take 3 $ map fst $ iterate (randLetter . snd) (randLetter $ mkSeed 1)

type Gen a = Seed -> (a,Seed)

randEven :: Gen Integer -- the output of rand * 2
randEven seed = (doubledValue,snd rndTupel)
  where
    rndTupel = rand seed
    doubledValue = (*) 2 $ fst rndTupel

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd seed = (modifiedValue,snd rndTupel)
  where
    rndTupel = randEven seed
    modifiedValue = (+) 1 $ fst rndTupel

randTen :: Gen Integer -- the output or rand * 10
randTen seed = (modifiedValue, snd rndTupel)
  where
    rndTupel = rand seed
    modifiedValue = (*) 10 $ fst rndTupel


randA :: (a -> a -> a) -> Gen a
randA f seed = (modifiedValue, snd rndTupel)
  where
    rndTupel = rand seed
    extractedValue = fst rndTupel
    modifiedValue = f extractedValue

randTen' :: Gen Integer
randTen' = randA (\x -> (*) x 10)
