Example of Genetic Algorithm
============================

We use the Haskell GA package

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> module GAexample where 
> 

> import Data.List (foldl')
> import System.Random (mkStdGen, random, randoms)
> 
> import GA (Entity(..), GAConfig(..), 
>            evolve, evolveVerbose, randomSearch)

Efficient sum: 

> sum' :: (Num a) => [a] -> a
> sum' = foldl' (+) 0

Efficient product: 

> product' :: (Num a) => [a] -> a
> product' = foldl' (*) 1

> type Bit = Integer

> list2vector :: [Integer] -> [Bit]
> list2vector xs = l2v 1 xs where 
>   l2v _ [] = []
>   l2v n (m:ms) = if n == m then 1 : l2v (n+1) ms
>                  else           0 : l2v (n+1) (m:ms)
> 
> vector2list :: [Bit] -> [Integer]
> vector2list bs = filter (/= 0) (zipWith (*) bs [1..])

> complement :: [Bit] -> [Bit]
> complement = map (flip mod 2 . succ)

> sumprod :: [Bit] -> (Integer,Integer)
> sumprod e = (n,m) where 
>   n =  sum' $ vector2list e
>   m =  product' $ vector2list $ complement e

GA Type Class implementation

> type Cards  = [Bit]

> instance Entity Cards Integer () Int IO where 
>
>  genRandom k seed = return (take k is)
>      where 
>         g = mkStdGen seed 
>         is = map (flip mod 2) $ randoms g
>
>  crossover n _ seed e1 e2 = let
>      k = seed `mod` (n+1)
>    in
>      return $ Just (take k e1 ++ drop k e2)
>
>  mutation n _ seed e = let
>      k = fst $ random (mkStdGen seed)
>      m = mod k n
>      xs = take m e
>      (y:ys) = drop m e
>      y' = mod (y+1) 2
>    in
>      return $ Just (xs ++ [y'] ++ ys) 
>
>  score' _ e = Just (abs (10*n - m)) where 
>      (n,m) = sumprod e 
>
>  isPerfect (_,s) = s == 0
            

> main :: IO() 
> main = do
>         let cfg = GAConfig 
>                     100 -- population size
>                     25  -- archive size (best entities to keep track of)
>                     100 -- maximum number of generations
>                     0.8 -- crossover rate (% of entities by crossover)
>                     0.2 -- mutation rate (% of entities by mutation)
>                     0.0 -- parameter for crossover (not used here)
>                     0.2 -- parameter for mutation (% of replaced bits)
>                     False -- whether or not to use checkpointing
>                     False -- don't rescore archive in each generation
> 
>             g = mkStdGen 0 -- random generator
>              
>        -- Do the evolution!
>         -- last parameter (extra data to score an entity) 
>         -- unused in this example
>         es <- evolveVerbose g cfg 40 ()
>         let e = snd $ head es :: [Integer]
>  
>         putStrLn $ "selected: " 
>                     ++ (show (vector2list e)) ++ show (sumprod e)

