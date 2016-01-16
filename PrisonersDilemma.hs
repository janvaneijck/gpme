module PrisonersDilemma where
import Data.List

type Move = Int
type Game = [Move]
type History = [Game]
type Strategy = [Move]  -- a strategy will be a list of moves where the positions correspond directly
                        -- to a possible history, from the list possibleHistory.

cc,cd,dc,dd :: Game
cc = [1,1]
cd = [1,0]
dc = [0,1]
dd = [0,0]

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
    iteratedMatch' n [pgame, ppgame, pppgame] (scoreP1, scoreP2) = let
            Just indexHistory       = elemIndex history (possibleHistory 3)
            moveP1                  = strat1 !! indexHistory
            moveP2                  = strat2 !! indexHistory
            (payoffP1, payoffP2)    = payoff [moveP1, moveP2]
            newscoreP1              = scoreP1 + payoffP1
            newscoreP2              = scoreP2 + payoffP2
            newhistory              = [moveP1, moveP2] : (take (length history - 1) history)
        in iteratedMatch' (n-1) newhistory (newscoreP1, newscoreP2)






