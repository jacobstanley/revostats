module Floorball where

import           Data.List
import           Data.Ord
import qualified Prelude as P
import           Prelude hiding ((-))

------------------------------------------------------------------------
-- Types

type Round = Int

-- | The team that played against Sharks
data Opponent = Dingoes | Kookaburras | ModDogs | Redbacks | Pirates | PiratesWhite
    deriving Show

-- | The round and opponent
data Game = Game Round Opponent
    deriving Show

-- | Stats for a team in one game
data GameStats = GameStats Game [(Player, ScoringStat)] [(Player, GoalieStat)]
    deriving Show

-- | Stats for one player over a season
data PlayerStats a = PlayerStats Player [(Game, a)]
    deriving Show

-- | Record for one player in one game
data Record a = Record Game Player a
    deriving Show

-- | A player represented by their name & number
data Player = Player
    { number :: Number
    , name :: Name
    } deriving (Show, Eq, Ord)

type Number = Int
type Name   = String

-- | Scoring-related stats
data ScoringStat = P Shots Goals Assists PIM GoalsF GoalsA
    deriving Show

-- | Goalie-related stats
data GoalieStat = G ShotsA GoalsA Assists PIM
    deriving Show

type Shots   = Int
type Goals   = Int
type Assists = Int
type Points  = Int
type PIM     = Int
type GoalsF  = Int
type GoalsA  = Int
type ShotsA  = Int

type ShotPct = Double
type SavePct = Double

-- | Create totals from a list of stats
class Total a where
    total :: [a] -> a

instance Total ScoringStat where
    total = foldl' go (P 0 0 0 0 0 0)
      where
        go (P s  g  a  pim  gf  ga)
           (P s' g' a' pim' gf' ga')
            =
            P (s + s') (g + g') (a + a') (pim + pim') (gf + gf') (ga + ga')

instance Total GoalieStat where
    total = foldl' go (G 0 0 0 0)
      where
        go (G s  g  a  pim)
           (G s' g' a' pim')
            =
            G (s + s') (g + g') (a + a') (pim + pim')

------------------------------------------------------------------------

-- | Extracts each player's scoring stats from a list of game stats
scoringStats :: [GameStats] -> [PlayerStats ScoringStat]
scoringStats = playerStats scorers

-- | Extracts each player's goalie stats from a list of game stats
goalieStats :: [GameStats] -> [PlayerStats GoalieStat]
goalieStats = playerStats goalies

-- | Extracts each player's stats from a list of game stats
playerStats :: (GameStats -> [(Player, a)]) -> [GameStats] -> [PlayerStats a]
playerStats f = groupByPlayer . concatMap (denormalize f)

groupByPlayer :: [Record a] -> [PlayerStats a]
groupByPlayer = map f . groupOn player . sortOn player
  where
    f xs@(x:_) = PlayerStats (player x) (map g xs)
    g (Record game _ stat) = (game, stat)

denormalize :: (GameStats -> [(Player, a)]) -> GameStats -> [Record a]
denormalize get gs@(GameStats game _ _) = map f (get gs)
  where
    f (player, stat) = Record game player stat

------------------------------------------------------------------------

scorers :: GameStats -> [(Player, ScoringStat)]
scorers (GameStats _ xs _) = xs

goalies :: GameStats -> [(Player, GoalieStat)]
goalies (GameStats _ _ xs) = xs

player :: Record a -> Player
player (Record _ p _) = p

------------------------------------------------------------------------

game :: Round -> Opponent -> [(Player, ScoringStat)] -> [(Player, GoalieStat)] -> GameStats
game rnd opp = GameStats (Game rnd opp)

(-) :: Player -> s -> (Player, s)
(-) = (,)

------------------------------------------------------------------------

-- | Calculates shot percentage given shots taken and goals scored
shotPct :: Shots -> Goals -> ShotPct
shotPct _ 0 = 0
shotPct s g | s >= g    = 100 * realToFrac g / realToFrac s
            | otherwise = error ("shotPct error: " ++ show g ++ " goals > " ++ show s ++ " shots")

-- | Calculates save percentage given shots against and goals against
savePct :: ShotsA -> GoalsA -> SavePct
savePct _ 0 = 1
savePct sa ga | sa >= ga  = realToFrac (sa P.- ga) / realToFrac sa
              | otherwise = error ("savePct error: " ++ show ga ++ " goals against > " ++ show sa ++ " shots against")

------------------------------------------------------------------------

-- | Sorts a list using the specified key
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

-- | Groups a list using the specified key
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)
