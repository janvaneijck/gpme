SGA
===

Jan van Eijck

> module SGA where

> import System.Random
> import Data.List


Get a random probability (a floating point number in the range 0..1):

> type Prob = Double 

> getRandomProb :: IO Prob
> getRandomProb = getStdRandom random

Same from a seed:

> type Seed = Int

> makeRandomProb :: Seed -> (Prob,StdGen)
> makeRandomProb seed = random (mkStdGen seed)

Generate a list of random seeds:

> makeSeeds :: Seed -> [Seed]
> makeSeeds seed = randoms (mkStdGen seed)             

Infinite stream of probabilities from seed

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

Use this to classify a list of probabilities according
to their quartiles. 
    
> countQuarts :: [Prob] -> [Count Int]
> countQuarts = freqCount . map classify

Now we can inspect the quality of our random generator of
probabilities. 

> prsQurts seed n = countQuarts $ prbs seed n

An example distribution: 

> distrib =
>   [0.1, 0.2, 0.05, 0.15, 0.0, 0.11, 0.07, 0.04, 0.0, 0.12, 0.16] :: [Prob]

This is indeed a distribution: 

> check = sum distrib == 1.0

We assume that the input of `roulette` is a list of probabilities that
sum to 1.  The output is an Int indicating the element from the
list that is selected.

> roulette :: Seed -> [Prob] -> Int
> roulette seed = select (head $ prs seed)

The selection function keeps track of the sizes of the slots on the
roulette wheel so that the item at position n has the slot (u,v],
where `u = sum [prob[0]..prob[n-1]]` and `v = sum [prob[0]..prob[n]]`.

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

> type Bit = Int

> mutation :: Seed -> Prob -> [Bit] -> [Bit]
> mutation seed prob bs = let
>    xs = zip bs (prs seed)
>    f (b,p) = if p < prob then (b + 1) `mod` 2 else b
>  in
>    map f xs

We aim at an efficient representation of populations with large
numbers of copies of the same bitlist. Type of a population:

> type Population = [Count [Bit]]

> display :: Population -> String
> display = concatMap displayIt where
>   displayIt (bs,n) = " " ++ concatMap show bs ++ " " ++ show n 

> popSize :: Population -> Int
> popSize = sum . (map snd)

From a population to a probability distribution:

> pop2dist :: Population -> [Prob]
> pop2dist pop = let
>    counts = map snd pop
>    total = sum counts
>  in
>    map (\x -> fromIntegral x / fromIntegral total) counts

Type of fitness function:

> type FF = [Bit] -> Double

We will assume that all fitness values are non-negative, and
that less is better, with 0 counting as a perfect fit.

Fitness distribution of a population: the first member of the
output gives the distribution, the second member the list of
indices of the bitlists with a perfect fit.         

> fitness :: FF -> Population -> ([Prob],[Int])
> fitness f pop = let
>    bss    = map fst pop
>    dist1  = pop2dist pop
>    values = map (\ (bs,_) -> f bs) pop
>    maxVal = maximum values
>    vs     = map (\x -> -x + maxVal + 1) values
>    total  = sum vs
>    dist2  = map (\ x -> x/total) vs
>    perfects = map snd $ filter (\ (x,_) -> x==0) (zip values [0..])
>  in
>    (distProduct dist1 dist2, perfects)
    
Product of two distributions of the same length:
take the product of the component pairs and renormalize. 

> distProduct :: [Prob] -> [Prob] -> [Prob]
> distProduct ps1 ps2 = let
>    ps12 = map (uncurry (*)) (zip ps1 ps2)
>    total = sum ps12
>  in
>    map (\x -> x/total) ps12

Pick a list of parents for forming the next generation, using the
roulette.

We do not assume here that population size and generation size
are equal, so we need a parameter `g` for the generation size,
so that population size minus `g` gives the number of survivors. 

> pickNextGen :: Seed -> FF -> Int -> Population -> ([[Bit]],[[Bit]],[[Bit]])
> pickNextGen seed f g pop
>  | g > popSize pop = error "generation size exceeds population"
>  | odd g           = error "generation size odd"                    
>  | otherwise = let
>      n         = popSize pop
>      (dist,pf) = fitness f pop
>      perfects  = map (fst . (pop!!)) pf
>      ks        = spin seed n dist
>      selected  = map (fst . (pop!!)) ks
>      (parents,survivors) = splitAt g selected
>    in
>      if perfects /= []
>        then ([], [], perfects)
>        else (parents,survivors,[])

Use a list of parents to create a new generation,
assuming a mutation probability: 

> nextGeneration :: Seed -> Prob -> [[Bit]] -> [[Bit]] -> Population
> nextGeneration seed mprob parents survivors = let
>     pairs     = pairup parents
>     n         = length (head parents)
>     ks        = nrsR (length pairs) (0,n-1)
>     nextgen   = unpair $ map (uncurry cross) (zip ks pairs)
>     newpairs  = zip (makeSeeds seed) nextgen
>     offspring = map (\ (x,bs) -> mutation x mprob bs) newpairs
>   in
>     freqCount (offspring ++ survivors)

This uses auxiliary functions for pairing up and unpairing. 
                 
> pairup :: [a] -> [(a,a)]
> pairup [] = []
> pairup [_] = []
> pairup (x:y:zs) = (x,y) : pairup zs

> unpair :: [(a,a)] ->[a]
> unpair [] = []
> unpair ((x,y):zs) = x:y: unpair zs

Create the next generation:
       
> nextGen :: Seed -> FF -> Prob -> Int -> Population -> (Bool,Population)
> nextGen seed f mprob g pop = let
>                [seed1,seed2] = take 2 $ makeSeeds seed
>                (parents,survivors,perfects) = pickNextGen seed1 f g pop
>              in 
>                if perfects /= []
>                   then (True,freqCount perfects)
>                   else (False,nextGeneration
>                                 seed2 mprob parents survivors)

 
Evolve a number of generations:

> evolve :: Int -> Seed -> FF -> Prob -> Int -> Population -> Population
> evolve 0 _ _ _ _ pop = pop
> evolve n seed f mprob g pop = let
>      [seed1,seed2] = take 2 $ makeSeeds seed
>      (perfct,newpop) = nextGen seed1 f mprob g pop
>    in
>      if perfct then newpop else evolve (n-1) seed2 f mprob g newpop

Verbose version:

> evolveVerbose :: Int -> Seed -> FF -> Prob -> Int -> Population -> IO ()
> evolveVerbose n seed f mprob g pop = evolveVb n n seed f mprob g pop where
>   evolveVb :: Int -> Int -> Seed -> FF -> Prob -> Int -> Population -> IO ()
>   evolveVb n 0 _ _ _ _ pop = do
>                             putStr (show n ++":")
>                             putStrLn (display pop)
>   evolveVb n k seed f mprob g pop = let
>      [seed1,seed2] = take 2 $ makeSeeds seed
>      (perfct,newpop) = nextGen seed1 f mprob g pop
>    in
>      do
>        putStr (show (n-k) ++":")
>        putStrLn (display pop)
>        if perfct then do
>                         putStr (show (n-k+1) ++ " Perfect fit:")
>                         putStrLn (display newpop)
>                   else evolveVb n (k-1) seed2 f mprob g newpop 

Evolve until a perfect fit is found (note that this may run forever): 

> evolveUntilPerfect :: Seed -> FF -> Prob -> Int -> Population -> IO ()
> evolveUntilPerfect = evolveVerbose (-1)

> ff1 :: FF
> ff1 bs = let
>     n = length bs
>     g x = (2^n - 1) - x
>  in
>     g $ fromBits bs 
   

> fromBits = fromBs . reverse
>  where 
>   fromBs [] = 0
>   fromBs (b:bs) = fromIntegral b + 2 * (fromBs bs) 

Another example:

> ff2 :: FF
> ff2 = fromBits

A third example:

> ff3 :: FF
> ff3 bs = abs (12345 - fromBits bs)  -- note: at least 14 bits needed for perfect fit

A fourth example:

> ff4 :: FF
> ff4 bs = let
>     n = length bs
>     m = quot n 3
>     (bs1,rest) = splitAt m bs
>     (bs2,bs3)  = splitAt m rest
>     x = fromBits bs1
>     y = fromBits bs2
>     z = fromBits bs3
>   in 
>     x^2 + y^2 + z^2

Example population: 
        
> population :: Population
> population = [([1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],100),([0,0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1],100)]
 
Random population of given bitlength and population size from seed:

> makeRandomPop :: Seed -> Int -> Int -> Population
> makeRandomPop seed bitsize popsize = let
>    seeds = take popsize (makeSeeds seed)
>    bitstrings = map (\ s -> take bitsize (nrsR s (0,1))) seeds
>  in
>    freqCount bitstrings
    
---

> fromBts :: [Bit] -> Integer
> fromBts = fromBs . reverse
>  where 
>   fromBs [] = 0
>   fromBs (b:bs) = fromIntegral b + 2 * (fromBs bs) 
