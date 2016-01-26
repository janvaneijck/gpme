SGA
===

Jan van Eijck

> module SGA where

> import System.Random
> import Data.List


> type Prob = Double 

Get a random probability (a floating point number in the range 0..1)
from a seed:

> type Seed = Int

> makeRandomProb :: Seed -> (Prob,StdGen)
> makeRandomProb seed = random (mkStdGen seed)

Generate a list of random seeds:

> makeSeeds :: Seed -> [Seed]
> makeSeeds seed = randoms (mkStdGen seed)             

Infinite stream of probabilities from seed:

> prs :: Seed -> [Prob]
> prs seed = randoms $ mkStdGen seed

Collect n random probability numbers in the range 0..1:

> prbs :: Seed -> Int -> [Prob]
> prbs seed n = take n $ randoms $ mkStdGen seed
 
Collect n probs in the range 0..1 and return their average: 

> avprbs :: Seed -> Int -> Prob
> avprbs seed n = (sum $ prbs seed n) / fromIntegral n 

Counting the numbers in the quartiles, for a list of floating point
numbers, supposedly each in the interval `[0..1)`:

> classify :: Prob -> Int
> classify p | p < 0.25  = 1
>            | p < 0.50  = 2 
>            | p < 0.75  = 3 
>            | otherwise = 4

Here is some useful code for our toolkit: 

> type Count a     = (a, Int)

> addCount :: Eq a => [Count a] -> Count a -> [Count a]
> addCount []     y    = [y]
> addCount (x:xs) y
>     | fst x == fst y = (fst x, snd x + snd y):xs
>     | otherwise      = x:addCount xs y

> freqCount :: Ord a => [a] -> [Count a]
> freqCount xs = -- sortBy (\ (x,_) (y,_) -> compare x y)
>                 (foldl' addCount [] [(x, 1) | x <- xs])

From a frequency list to a list:

> freq2list :: Ord a => [Count a] -> [a]
> freq2list = concatMap (\ (x,n) -> (take n $ repeat x)) 
        
Next, given a probability value and a probability distribution we want
to select the portion of the roulette wheel that holds this value. So
the selection function for our roulette keeps track of the sizes of
the slots on the roulette wheel so that the item at position n has the
slot (u,v], where `u = sum [prob[0]..prob[n-1]]` and `v = sum
[prob[0]..prob[n]]`.

> select :: Double -> [Prob] -> Int
> select x xs = sel 0 0 xs where
>  sel n u [v] = if x <= u+v then n else error "wrong data"
>  sel n u (v:vs) = if  x <= u+v then n else sel (n+1) (u+v) vs

Spin the roulette wheel n times and collect the results. 

> spin :: Seed -> Int -> [Prob] -> [Int]
> spin seed n distr =
>    map (\x -> select x distr) (prbs seed n)

Now let's check the results by repeating the spinning procedure
10000 times.

If we compute new probabilities from the frequency count, then the
result should be close to the probabilities that we started out with.

> freq2prob :: [Count a] -> [(a,Prob)]
> freq2prob freqs = let
>     total = sum $ map snd freqs
>   in
>     map (\ (x,n) -> (x,(fromIntegral n/fromIntegral total))) freqs

Glueing the two together: 

> probCount :: Ord a => [a] -> [(a,Prob)]
> probCount = freq2prob . freqCount

> checkFrq :: Seed -> Int -> [Prob] -> [(Int,Prob)]
> checkFrq seed n dist = probCount $ spin seed n dist

Now it is easy to see that the results are very close to the input
probabilities.

Generate an infinite list of numbers in a specified interval.
         
> nrsR :: Seed -> (Int,Int) -> [Int]
> nrsR seed (lo,hi) = randomRs (lo,hi) (mkStdGen seed)

Test for 10000 numbers between 3 and 12.

> testNrs :: Seed -> [Count Int]
> testNrs seed = freqCount $ take 10000 $ nrsR seed (3,12)

Crossover: no need to restrict the type to bit lists.

> crossover :: Int -> ([a],[a]) -> ([a],[a])
> crossover k (xs,ys) = let
>     (xs1,xs2) = splitAt k xs
>     (ys1,ys2) = splitAt k ys
>    in
>      (xs1++ys2, ys1++xs2)

We can also build in some constraints:

> cross :: Int -> ([a],[a]) -> ([a],[a])
> cross k (xs,ys) = if length xs /= length ys then error "different lengths"
>                   else if k < 0 then error "negative index"
>                   else if k >= length xs then error "index too large"
>                   else crossover k (xs,ys)

We could use bit strings as our chromosomes, but we can easily
generalize this by adding a modulus parameter:

> type Allele     = Int
> type Chromosome = [Allele]
> type Mod        = Int

> fromChromosome :: Mod -> Chromosome -> Integer
> fromChromosome m = fromC (fromIntegral m) . reverse where
>   fromC _ [] = 0
>   fromC m (a:as) = fromIntegral a + m * (fromC m as)

> toChromosome :: Mod -> Integer -> Chromosome
> toChromosome m = reverse . toC (fromIntegral m) where
>   toC _ 0 = []
>   toC m n = let (q,r) = quotRem n m
>             in fromIntegral r : toC m q
 
Mutation for chromosomes: mutated alleles get a random new value. 

> mutation :: Seed -> Prob -> Mod -> Chromosome -> Chromosome
> mutation seed prob m as = let
>    [seed1,seed2] = take 2 $ makeSeeds seed
>    xs = zip as (zip (prs seed1) (nrsR seed2 (1,m-1)))
>    f (a,(p,k)) = if p < prob then (a + k) `mod` m else a
>  in
>    map f xs

Type of a population:

> type Population = [Chromosome]

> display :: Population -> String
> display = concatMap displayIt where
>   displayIt bs = " " ++ concatMap show bs

> popSize :: Population -> Int
> popSize = length 

Type of fitness function:

> type FF = Mod -> Chromosome -> Integer 

We will assume that all fitness values are non-negative, and
that less is better, with 0 counting as a perfect fit.

Sometimes it is useful to powerboost a fitness function, while bearing
in mind that closer to 0 means closer to perfect. 

> powerBoost :: FF -> Float -> FF
> powerBoost f k m x = let
>     y = 1 + fromIntegral (f m x)
>   in
>     ceiling $ logBase k y

Fitness distribution of a population: the first member of the
output gives the distribution, the second member the list of
indices of the bitlists with a perfect fit.         

> fitness :: FF -> Mod -> Population -> ([Prob],Population)
> fitness f m pop = let
>    values = map (f m) pop
>    maxVal = maximum values
>    vs     = map (\x -> -x + maxVal + 1) values
>    total  = sum vs
>    dist   = map (\ x -> fromIntegral x/ fromIntegral total) vs
>    perfects = filter (\ x -> f m x == 0) pop
>  in
>    (dist, perfects)
    
Pick the `m` best elements from a population (not used right now in
the implementation of pickNextGen). 

> pickBest :: FF -> Mod -> Int -> Population -> Population
> pickBest f m k candidates = let 
>    compareFct x y = if f m x < f m y then LT
>                     else if f m x == f m y then EQ
>                     else GT
>    sortedpop = sortBy compareFct candidates
>  in
>    take k $ sortedpop
      
Pick a list of parents for forming the next generation, using the
roulette.

We do not assume here that population size and generation size
are equal, so we need a parameter `g` for the generation size,
so that population size minus `g` gives the number of survivors.

For these survivors we do also use the roulette.  An alternative would
be to simply pick the best elements from the population for the
parents (or for the survivors).

> pickNextGen :: Seed
>                -> FF
>                -> Mod
>                -> Int       
>                -> Population
>                -> (Population,Population,Population)
> pickNextGen seed f m k pop
>  | k > popSize pop = error "generation size exceeds population"
>  | odd k           = error "generation size odd"                    
>  | otherwise = let
>      n         = popSize pop
>      (dist,perfects) = fitness f m pop
>      ks        = spin seed n dist
>      selected  = map (pop!!) ks
>      (parents,survivors) = splitAt k selected
> --    parents = pickBest f m g pop
> --    survivors = pickBest f m (n-g) pop
>    in
>      if perfects /= []
>        then ([], [], perfects)
>        else (parents,survivors,[])

Use a list of parents to create a new generation,
assuming a mutation probability: 

> nextGeneration :: Seed
>                   -> Mod
>                   -> Prob
>                   -> Population
>                   -> Population
>                   -> Population
> nextGeneration seed m mprob parents survivors = let
>     pairs     = pairup parents
>     n         = length (head parents)
>     ks        = nrsR (length pairs) (0,n-1)
>     nextgen   = unpair $ map (uncurry cross) (zip ks pairs)
>     newpairs  = zip (makeSeeds seed) nextgen
>     offspring = map (\ (x,bs) -> mutation x mprob m bs) newpairs
>   in
>     offspring ++ survivors

This uses auxiliary functions for pairing up and unpairing. 
                 
> pairup :: [a] -> [(a,a)]
> pairup [] = []
> pairup [_] = []
> pairup (x:y:zs) = (x,y) : pairup zs

> unpair :: [(a,a)] ->[a]
> unpair [] = []
> unpair ((x,y):zs) = x:y: unpair zs

Create the next generation:
       
> nextGen :: Seed
>            -> FF
>            -> Mod
>            -> Prob
>            -> Int
>            -> Population
>            -> (Bool,Population)
> nextGen seed f m mprob g pop = let
>   [seed1,seed2] = take 2 $ makeSeeds seed
>   (parents,survivors,perfects) = pickNextGen seed1 f m g pop
>  in 
>   if perfects /= []
>     then (True,perfects)
>     else (False,nextGeneration
>                 seed2 m mprob parents survivors)

 
Evolve a number of generations:

> evolve :: Int
>           -> Seed
>           -> FF
>           -> Mod
>           -> Prob
>           -> Int
>           -> Population
>           -> Population
> evolve 0 _ _ _ _ _ pop = pop
> evolve n seed f m mprob g pop = let
>      [seed1,seed2] = take 2 $ makeSeeds seed
>      (perfct,newpop) = nextGen seed1 f m mprob g pop
>    in
>      if perfct then newpop else evolve (n-1) seed2 f m mprob g newpop

Verbose version:

> evolveVerbose :: Int
>                  -> Seed
>                  -> FF
>                  -> Mod
>                  -> Prob
>                  -> Int
>                  -> Population
>                  -> IO ()
> evolveVerbose n seed f m mprob g pop =
>   evolveVb n n seed f m mprob g pop where
>   evolveVb :: Int
>               -> Int
>               -> Seed
>               -> FF
>               -> Mod
>               -> Prob
>               -> Int
>               -> Population
>               -> IO ()
>   evolveVb n 0 _ _ _ _ _ pop = do
>                             putStr (show n ++":")
>                             putStrLn (display pop)
>   evolveVb n k seed f m mprob g pop = let
>      [seed1,seed2] = take 2 $ makeSeeds seed
>      (perfct,newpop) = nextGen seed1 f m mprob g pop
>    in
>      do
>        putStr (show (n-k) ++":")
>        putStrLn (display pop)
>        if perfct then do
>                         putStr (show (n-k+1) ++ " Perfect fit:")
>                         putStrLn (display $ [head newpop])
>                   else evolveVb n (k-1) seed2 f m mprob g newpop 

Evolve until a perfect fit is found (note that this may run forever): 

> evolveUntilPerfect :: Seed
>                       -> FF
>                       -> Mod
>                       -> Prob
>                       -> Int
>                       -> Population
>                       -> IO ()
> evolveUntilPerfect = evolveVerbose (-1)

> ff1 :: FF
> ff1 m as = let
>     n = length as
>     g x = ((fromIntegral m)^n - 1) - x
>  in
>     g $ fromChromosome m as
  
Another example:

> ff2 :: FF
> ff2 = fromChromosome

A third example:

> ff3 :: FF
> ff3 m as = abs (12345 - fromChromosome m as)

Note that at least 14 bits are needed for perfect fit, but shorter
chromosomes will do if we increase the modulus. 

A fourth example:

> ff4 :: FF
> ff4 m as = let
>     n = length as
>     k = quot n 3
>     (as1,rest) = splitAt k as
>     (as2,as3)  = splitAt k rest
>     x = fromChromosome m as1
>     y = fromChromosome m as2
>     z = fromChromosome m as3
>   in 
>     x^2 + y^2 + z^2

Example population: 
        
> population :: Population
> population = freq2list
>               [([1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0],100),
>                ([0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1],100)]
 
Random population of given bitlength and population size from seed:

> makeRandomPop :: Seed -> Mod -> Int -> Int -> Population
> makeRandomPop seed m chromsize popsize = let
>    seeds = take popsize (makeSeeds seed)
>  in
>    map (\ s -> take chromsize (nrsR s (0,m-1))) seeds
    
---

> main = do
>          let thispopsize = 200
>              thischrsize = 12
>              thisgensize = 30
>              thisnr      = 10000
>              thisff      = powerBoost ff4 3
>              thismod     = 6
>              thismprob   = 0.2
>          seed1       <- getStdRandom random
>          seed2       <- getStdRandom random            
>          let thispop = makeRandomPop seed1 thismod thischrsize thispopsize
>          evolveVerbose thisnr seed2 thisff thismod thismprob thisgensize
>                        thispop

---
