#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.List hiding (head, span)
import qualified Prelude as P
import           Prelude hiding (head, id, div, span, (-))
import qualified Text.Blaze.Html4.Strict as H
import           Text.Blaze.Html4.Strict hiding (map, title)
import           Text.Blaze.Html4.Strict.Attributes hiding (name, span, style)
import           Text.Blaze.Renderer.String (renderHtml)
import           Text.Printf

--
-- Games
--

games =
    [ Game 1 Dingoes
        --           S  G  A PIM GF GA
      [ collopy - P  8  0  1  0  4  4
      , basterA - P  1  0  0  0  0  0
      , emms    - P  3  2  0  2  4  3
      , obrien  - P  2  1  0  0  4  4
      , yeoh    - P  5  1  2  0  4  4
      , diffen  - P  2  0  0  0  2  3
      , tng     - P  2  0  0  0  2  2
      ] --           SA GA A PIM
      [ stanley - G  27 4  0  0
      ]

    , Game 2 Redbacks
        --            S  G  A PIM GF GA
      [ collopy  - P  2  1  0  0  3  2
      , basterA  - P  2  1  1  0  3  2
      , emms     - P  0  0  0  0  0  2
      , obrien   - P  2  0  0  0  1  2
      , mitchell - P  2  0  0  0  1  2
      , yeoh     - P  2  0  0  0  2  0
      , diffen   - P  1  0  0  0  3  0
      , tng      - P  1  1  0  5  2  0
      ] --            SA GA A PIM
      [ stanley  - G  29 2  0  0
      ]

    ]

--
-- Players
--

collopy  = Player 4  "Kris Collopy"
basterA  = Player 6  "Adrian Baster"
emms     = Player 7  "Morgan Emms"
obrien   = Player 10 "Don O'Brien"
ebeling  = Player 12 "Kade Ebeling"
basterP  = Player 13 "Patrick Baster"
mitchell = Player 17 "Ben Mitchell"
broome   = Player 19 "Steve Broome"
keogh    = Player 21 "Tara Keogh"
blades   = Player 23 "Justin Blades"
yeoh     = Player 24 "Nicholas Yeoh"
diffen   = Player 25 "Oli Diffen"
salt     = Player 32 "Matthew Salt"
tng      = Player 38 "Melvin Tng"
king     = Player 77 "Steve King"
stanley  = Player 91 "Jacob Stanley"

--
-- Data types
--

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

--
-- Magic Voodoo Spells
--

main = putStrLn $ renderHtml $ docTypeHtml $ do
    head $ do
        H.title $ "Revolution Sharks Statistics"
        style ! type_ "text/css" $ string $ "\n" ++ intercalate "\n" css ++ "\n"
    body $ do
        h1 "Revolution Sharks Statistics"
        h2 "Games"
        mapM_ game games
        h2 "Players"
        pre $ string $ show $ xs
  where
    css = [ "table   { margin-top: 0.5em; }"
          , "td, th  { padding: 0 0.5em; }"
          , ".text   { text-align: left; }"
          , ".number { text-align: right; }"
          ]

    xs = map (P.head . players) games


game :: Game -> Html
game (Game rnd opp ps gs) = do
    h3 $ string $ "Round " ++ show rnd ++ " (vs " ++ show opp ++ ")"
    table $ do
        gamePlayerHeader
        gamePlayerEntries ps
    table $ do
        gameGoalieHeader
        gameGoalieEntries gs
--
-- Player stats
--

gamePlayerHeader :: Html
gamePlayerHeader = tr $ do
    ths "#"      "Number"
    ths "Player" "Player"
    thn "G"      "Goals"
    thn "A"      "Assists"
    thn "P"      "Points"
    thn "S"      "Shots"
    thn "S%"     "Shot Percentage"
    thn "+/-"    "Plus/Minus"
    thn "PIM"    "Penalties In Minutes"

gamePlayerEntries :: [(Player, PStat)] -> Html
gamePlayerEntries = mapM_ entry
  where
    entry (p, P s g a pim gf ga) = tr $ do
        tds (show $ number p)
        tds (name p)
        tdi g
        tdi a
        tdi (g + a)
        tdi s
        tdn $ printf "%.1f" $ shotPct s g
        tdi (gf P.- ga)
        tdi pim

--
-- Goalie stats
--

gameGoalieHeader :: Html
gameGoalieHeader = tr $ do
    ths "#"      "Number"
    ths "Goalie" "Goalie"
    thn "SA"     "Shots Against"
    thn "GA"     "Goals Against"
    thn "Sv"     "Saves"
    thn "Sv%"    "Save Percentage"
    thn "A"      "Assists"
    thn "PIM"    "Penalties In Minutes"

gameGoalieEntries :: [(Player, GStat)] -> Html
gameGoalieEntries = mapM_ entry
  where
    entry (p, G sa ga a pim) = tr $ do
        tdi (number p)
        tds (name p)
        tdi sa
        tdi ga
        tdi (sa P.- ga)
        tdn $ drop 1 $ printf "%.3f" $ savePct sa ga
        tdi a
        tdi pim

--
-- Helpers
--

shotPct :: Shots -> Goals -> ShotPct
shotPct _ 0 = 0
shotPct s g | s >= g    = 100 * realToFrac g / realToFrac s
            | otherwise = error ("shotPct error: " ++ show g ++ " goals > " ++ show s ++ " shots")

savePct :: ShotsA -> GoalsA -> SavePct
savePct _ 0 = 1
savePct sa ga | sa >= ga  = realToFrac (sa P.- ga) / realToFrac sa
              | otherwise = error ("savePct error: " ++ show ga ++ " goals against > " ++ show sa ++ " shots against")

ths = th' "text"
thn = th' "number"
th' cls abbrev full = th ! class_ cls ! title full $ abbrev

tdi x = td ! class_ "number" $ int x
tds x = td ! class_ "text"   $ string x
tdn x = td ! class_ "number" $ string x

int :: Int -> Html
int = string . show
