
module Set1 where

import MCPrelude


fiveRands :: [Integer]
fiveRands = take 5 (map fst (iterate (rand . snd) (rand $ mkSeed 1)))


randLetter :: Gen Char
randLetter seed = (rndchar,seed')
  where
    (rnd,seed') = rand seed
    rndchar    = toLetter rnd


randString3 :: String
randString3 = take 3 $ map fst $ iterate (randLetter . snd) (randLetter $ mkSeed 1)


type Gen a = Seed -> (a,Seed)

randEven :: Gen Integer -- the output of rand * 2
randEven seed = (doubledValue,seed')
  where
    (rnd,seed') = rand seed
    doubledValue = (*) 2 rnd

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd seed = (modifiedValue,seed')
  where
    (rnd,seed') = randEven seed
    modifiedValue = (+) 1 rnd

randTen :: Gen Integer -- the output of rand * 10
randTen seed = (modifiedValue, seed')
  where
    (rnd,seed') = rand seed
    modifiedValue = (*) 10 rnd


randA :: (a -> b) -> Gen a -> Gen b
randA f gen = \s -> (f $ fst $ gen s, snd $ gen s)


randEven' :: Gen Integer
randEven' = randA (* 2) rand

randOdd' :: Gen Integer
randOdd' = randA (+ 1) randEven'

randTen' :: Gen Integer
randTen' = randA (* 10) rand


valuesList :: [Integer]
valuesList = map fst [randEven' $ mkSeed 1,randOdd' $ mkSeed 1, randTen' $ mkSeed 1]

randPair :: Gen (Char, Integer) -- :: Seed -> ((Char,Integer),Seed)
randPair seed = ((rndLet,rnd),seed'')
  where
    (rndLet,seed') = randLetter seed
    (rnd,seed'')   = rand seed'


generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair genA genB = \s -> let (rndA,s1) = genA s
                                  (rndB,s2) = genB s1
                              in ((rndA,rndB),s2)

generalB ::(a -> b -> c) ->  Gen a -> Gen b -> Gen c
generalB construct genA genB = \s -> let (rndA,s1) = genA s
                                         (rndB,s2) = genB s1
                                      in ((construct rndA rndB),s2)

generalPair2 :: Gen a -> Gen b -> Gen (a,b)
generalPair2 genA genB = generalB (,) genA genB

repRandom :: [Gen a] -> Gen [a]
repRandom lst = \s -> repRandomHelper lst [] s
  where
    repRandomHelper [] acc s     = (reverse acc,s)
    repRandomHelper (x:xs) acc s = let (rnd,s1) = x s
                                   in repRandomHelper xs (rnd:acc) s1
