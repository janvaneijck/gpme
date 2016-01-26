SGA
===

Jan van Eijck

> module TestSGA where
> import System.Random
> import Data.List
> import SGA
> import Test.QuickCheck

> prop_nrsR seed =
>   let xs  = take 10000 $ nrsR seed (1,2)
>       fr  = freqCount xs
>       k   = snd $ head fr
>   in
>       4800 < k && k < 5200

> prop_spin seed =
>   let xs = spin seed 10000 [1/4,1/2,1/8,1/8]
>       fr = freqCount xs
>       Just k  = lookup 0 fr
>   in
>       2350 < k && k < 2650

> prop_fromtoChrom m =
>   let n =   fromIntegral (m `mod` 500)
>       chr = [1..(n-1)]
>       k   = fromChromosome n chr
>   in chr == toChromosome n k
