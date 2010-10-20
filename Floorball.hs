module Floorball where


data Game = Game
    { round    :: Round
    , opponent :: Opponent
    , players  :: [(Player, PStat)]
    , goalies  :: [(Player, GStat)]
    }
    deriving Show

(-) :: Player -> s -> (Player, s)
(-) = (,)

type Round = Int

data Opponent = Dingoes | Kookaburras | ModDogs | Redbacks | Pirates | PiratesWhite
    deriving Show

data PStat = P Shots  Goals  Assists PIM GoalsF GoalsA
    deriving Show

data GStat = G ShotsA GoalsA Assists PIM
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

data Player = Player { number :: Number, name :: Name }
    deriving (Show, Eq)

type Number = Int
type Name   = String
