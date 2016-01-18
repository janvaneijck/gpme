SGA
===

Jan van Eijck

> module SGA where

> import System.Random
> import Data.List


Get a random probability (a floating point number in the range 0..1):

> getRandomProb :: IO Float
> getRandomProb = getStdRandom random

Same from a seed:

> makeRandomProb :: Int -> (Float,StdGen)
> makeRandomProb seed = random (mkStdGen seed)

Generate a list of random seeds:

> makeSeeds :: Int -> [Int]
> makeSeeds seed = randoms (mkStdGen seed)             

Infinite stream of probabilities from seed

> prs :: Int -> [Float]
> prs seed = randoms $ mkStdGen seed

Collect n random probability numbers in the range 0..1:

> prbs :: Int -> Int -> [Float]
> prbs seed n = take n $ randoms $ mkStdGen seed
 

Collect n probs in the range 0..1 and return their average: 

> avprbs :: Int -> Int -> Float
> avprbs seed n = (sum $ prbs seed n) / fromIntegral n 

Counting the numbers in the quartiles, for a list of floating point
numbers, supposedly each in the interval `[0..1)`:

> classify :: Float -> Int
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
    
> countQuarts :: [Float] -> [Count Int]
> countQuarts = freqCount . map classify

Now we can inspect the quality of our random generator of
probabilities. 

> prsQurts seed n = countQuarts $ prbs seed n

An example distribution: 

> distrib =
>   [0.1, 0.2, 0.05, 0.15, 0.0, 0.11, 0.07, 0.04, 0.0, 0.12, 0.16] :: [Float]

This is indeed a distribution: 

> check = sum distrib == 1.0

We assume that the input of `roulette` is a list of probabilities that
sum to 1.  The output is an Int indicating the element from the
list that is selected.

> roulette :: Int -> [Float] -> Int
> roulette seed = select (head $ prs seed)

The selection function keeps track of the sizes of the slots on the
roulette wheel so that the item at position n has the slot (u,v],
where `u = sum [prob[0]..prob[n-1]]` and `v = sum [prob[0]..prob[n]]`.

> select :: Float -> [Float] -> Int
> select x xs = sel 0 0 xs where
>  sel n u [v] = if x <= u+v then n else error "wrong data"
>  sel n u (v:vs) = if  x <= u+v then n else sel (n+1) (u+v) vs

Spin the roulette wheel n times and collect the results. 

> spin :: Int-> Int -> [Float] -> [Int]
> spin seed n distr =
>    map (\x -> select x distr) (prbs seed n)

Now let's check the results by repeating the spinning procedure
10000 times.

If we compute new probabilities from the frequency count, then the
result should be close to the probabilities that we started out with.

> freq2prob :: [Count a] -> [(a,Float)]
> freq2prob freqs = let
>     total = sum $ map snd freqs
>   in
>     map (\ (x,n) -> (x,(fromIntegral n/fromIntegral total))) freqs

Glueing the two together: 

> probCount :: Ord a => [a] -> [(a,Float)]
> probCount = freq2prob . freqCount

> checkFrq :: Int -> Int -> [Float] -> [(Int,Float)]
> checkFrq seed n dist = probCount $ spin seed n dist

Now it is easy to see that the results are very close to the input
probabilities.

Generate an infinite list of numbers in a specified interval.
         
> nrsR :: Int -> (Int,Int) -> [Int]
> nrsR seed (lo,hi) = randomRs (lo,hi) (mkStdGen seed)

Test for 10000 numbers between 3 and 12.

> testNrs :: Int -> [Count Int]
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

> mutation :: Int -> Float -> [Bit] -> [Bit]
> mutation seed prob bs = let
>    xs = zip bs (prs seed)
>    f (b,p) = if p < prob then (b + 1) `mod` 2 else b
>  in
>    map f xs

We aim at an efficient representation of populations with large
numbers of copies of the same bitlist. Type of a population:

> type Population = [Count [Bit]]
> 
> popSize :: Population -> Int
> popSize = sum . (map snd)

Example population from the Goldberg assignment:
        
> population :: Population
> population = [([1,1,1,0,0,0,0,0,0],100),([0,0,0,1,1,0,0,0,0],100)]
 
From a population to a probability distribution:

> pop2dist :: Population -> [Float]
> pop2dist pop = let
>    counts = map snd pop
>    total = sum counts
>  in
>    map (\x -> fromIntegral x / fromIntegral total) counts

Type of fitness function:

> type FF = [Bit] -> Float 

We will assume that all fitness values are non-negative, and
that less is better, with 0 counting as a perfect fit.

Fitness distribution of a population: the first member of the
output gives the distribution, the second member the list of
indices of the bitlists with a perfect fit.         

> fitness :: FF -> Population -> ([Float],[Int])
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
    
Product of two distributions:

> distProduct :: [Float] -> [Float] -> [Float]
> distProduct ps1 ps2 = let
>    ps12 = map (uncurry (*)) (zip ps1 ps2)
>    total = sum ps12
>  in
>    map (\x -> x/total) ps12

Pick an element from a population using a roulette to take frequency
and fitness of elements into account.


> pickRandom :: Int -> FF -> Population -> [Bit]
> pickRandom seed f pop = let
>    dist = fst (fitness f pop)
>    k    = roulette seed dist       
>  in 
>    fst (pop!!k)

Pick a list of parents for forming the next generation, using the
roulette. We assume here that population size and generation size
are equal. 

> pickParents :: Int -> FF -> Population -> ([[Bit]],[[Bit]])
> pickParents seed f pop = let
>      (dist,perfects) = fitness f pop
>      n    = popSize pop  -- same as generation size
>      ks   = spin seed n dist       
>    in 
>      if perfects /= []
>        then ([], map (fst . (pop!!)) perfects)
>        else (map (fst . (pop!!)) ks, [])

Use a list of parents to create a new generation,
assume a mutation probability: 

> nextGeneration :: Int -> Float -> [[Bit]] -> Population
> nextGeneration seed mprob bss = let
>     pairs    = pairup bss
>     n        = length (head bss)
>     ks       = nrsR (length pairs) (0,n-1)
>     nextgen  = unpair $ map (uncurry cross) (zip ks pairs)
>     newpairs = zip (makeSeeds seed) nextgen
>     mutgen   = map (\ (x,bs) -> mutation x mprob bs) newpairs
>   in
>     freqCount mutgen

This uses auxiliary functions for pairing up and unpairing. 
                 
> pairup :: [a] -> [(a,a)]
> pairup [] = []
> pairup [_] = []
> pairup (x:y:zs) = (x,y) : pairup zs

> unpair :: [(a,a)] ->[a]
> unpair [] = []
> unpair ((x,y):zs) = x:y: unpair zs

Create the next generation:
       
> nextGen :: Int -> FF -> Float -> Population -> Population
> nextGen seed f mprob pop = let
>                [seed1,seed2] = take 2 $ makeSeeds seed
>                (ps,perfects) = pickParents seed1 f pop
>              in 
>                if perfects /= []
>                   then freqCount perfects
>                   else nextGeneration seed2 mprob ps

 
Evolve a number of generations:

> evolve :: Int -> FF -> Float -> Population -> Int -> Population
> evolve _ _ _ pop 0 = pop
> evolve seed f mprob pop n = let
>      [seed1,seed2] = take 2 $ makeSeeds seed
>      newpop = nextGen seed1 f mprob pop
>    in
>      evolve seed2 f mprob newpop (n-1)

Example fitness function:

> ff1 :: FF
> ff1 bs = let
>     n = length bs
>     g x = (2^n - 1) - x
>  in
>     g $ fromBits bs 
   

> fromBits = fromBs . reverse           
> 
> fromBs [] = 0
> fromBs (0:bs) = 2 * (fromBs bs)
> fromBs (1:bs) = 2 * (fromBs bs) + 1

Another example:

> ff2 :: FF
> ff2 = fromBits

---
