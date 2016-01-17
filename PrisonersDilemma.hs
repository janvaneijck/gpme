module PrisonersDilemma where
import Data.List
import System.Random
import GPME_mfga_v2 (evolve,evolveVerbose,freqRep) 

type Move = Char
type Game = [Move]
type History = [Game]
type Strategy = [Move]  -- a strategy will be a list of moves where the positions correspond directly
                        -- to a possible history, from the list possibleHistory.
type Organism = String

cc,cd,dc,dd :: Game
cc = ['1','1']
cd = ['1','0']
dc = ['0','1']
dd = ['0','0']

-- possibleHistory takes an input representing the amount of games stored in the history and returns
-- all possible histories. The order of this list will be used as a reference for the organisms in the
-- genetic algorithm.
possibleHistory :: Int -> [History]
possibleHistory n = sequence $ replicate n [cc,cd,dc,dd]

-- payoff gives, given a game, the corresponding payoff
payoff :: Fractional a => Game -> (a,a)
payoff game
    | game == cc = (3,3)
    | game == cd = (0,5)
    | game == dc = (5,0)
    | game == dd = (1,1)
    | otherwise  = error "payoff ERROR: not a correctly defined game"

-- iteratedMatch takes as input the amount of iterations, two strategies and a (hypothetical) history
-- on which to play and plays the Prisoner's Dilemma for the given amount of iterations, using the first
-- strategy for player 1 and the second strategy for player 2 and returns the total payoff for both players.
iteratedMatch :: Fractional a => Int -> Strategy -> Strategy -> History -> (a,a)
iteratedMatch n strat1 strat2 history = iteratedMatch' n history (0,0) where
    iteratedMatch' 0 _ score = score
    iteratedMatch' n history (scoreP1, scoreP2) = let
            Just indexHistory       = elemIndex history (possibleHistory $ length history)
            moveP1                  = strat1 !! indexHistory
            moveP2                  = strat2 !! indexHistory
            (payoffP1, payoffP2)    = payoff [moveP1, moveP2]
            newscoreP1              = scoreP1 + payoffP1
            newscoreP2              = scoreP2 + payoffP2
            newhistory              = [moveP1, moveP2] : (take (length history - 1) history)
        in iteratedMatch' (n-1) newhistory (newscoreP1, newscoreP2)

-- this is the fitness function that tests a strategy against all strategies in the current population, using its
-- own hypothetical three previous matches
fitnessPD :: [(String,Int)] -> String -> Float
fitnessPD pop (g1P1:g1P2:g2P1:g2P2:g3P1:g3P2:stratP1) = recurse pop where
    starthistory = [[g1P1,g1P2],[g2P1,g2P2],[g3P1,g3P2]]
    recurse [] = 0
    recurse ((stratP2,_):rest) = (fst $ iteratedMatch 100 stratP1 (drop 6 stratP2) starthistory) + recurse rest

-- intToChar converts a bit from type Int to type Char
intToChar :: Int -> Char
intToChar 0 = '0'
intToChar 1 = '1'
intToChar _ = undefined

-- startPopulation takes a seed, an organism size and a population size and creates a random population of bitstrings
-- of the right size using the given seed
startPopulation :: Int -> Int -> Int -> [(Organism,Int)]
startPopulation seed stringsize popsize = let
        seeds = map (intToChar . flip (mod) 2) (take (stringsize * popsize) (randoms $ mkStdGen seed :: [Int]))
    in freqRep $ startPopulation' seeds where
        startPopulation' [] = []
        startPopulation' seeds = take stringsize seeds : startPopulation' (drop stringsize seeds)

-- randomStrat takes a seed and uses it to create a single strategy of length 64
randomStrat :: Int -> Organism
randomStrat seed = map (intToChar . flip (mod) 2) (take (64) (randoms $ mkStdGen seed :: [Int]))

-- titfortatP2 takes the size of the history and returns the tit for tat strategy for player 2
titfortatP2 :: Int -> Strategy
titfortatP2 sizehist = titfortat (sizestring - 1) where
        sizestring = 4^sizehist 
        titfortat 0 = let prevgame = head $ (!!) (possibleHistory sizehist) (sizestring - 1) in 
            if head prevgame == '0' then ['0'] else ['1']
        titfortat n = let prevgame = head $ (!!) (possibleHistory sizehist) (sizestring - 1 - n) in 
            if head prevgame == '0' then '0' : titfortat (n-1) else '1' : titfortat (n-1)