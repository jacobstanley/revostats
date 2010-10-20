#!/usr/bin/runghc
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.List hiding (head, span)
import           Prelude hiding (head, id, div, span)
import           Text.Blaze.Html4.Strict hiding (map, title)
import qualified Text.Blaze.Html4.Strict as H
import           Text.Blaze.Html4.Strict.Attributes hiding (name, span, style)
import           Text.Blaze.Renderer.Pretty (renderHtml)
import           Text.Printf

--
-- Games
--

games =
    [ Game 1 Dingoes
        --         Sh G  A PIM GF GA
      [ P collopy  8  0  1  0  4  4
      , P basterA  1  0  0  0  0  0
      , P emms     3  2  0  2  4  3
      , P obrien   2  1  0  0  4  4
      , P yeoh     5  1  2  0  4  4
      , P diffen   2  0  0  0  2  3
      , P tng      2  0  0  0  2  2
        --         SA GA A PIM
      , G stanley  27 4  0  0
      ]

    , Game 2 Redbacks
        --         Sh G  A PIM GF GA
      [ P collopy  2  1  0  0  3  2
      , P basterA  2  1  1  0  3  2
      , P emms     0  0  0  0  0  2
      , P obrien   2  0  0  0  1  2
      , P mitchell 2  0  0  0  1  2
      , P yeoh     2  0  0  0  2  0
      , P diffen   1  0  0  0  3  0
      , P tng      1  1  0  5  2  0
        --         SA GA A PIM
      , G stanley  29 2  0  0
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

data Game = Game Round Opponent [Entry]
    deriving Show

type Round = Int

data Opponent = Dingoes | Kookaburras | ModDogs | Redbacks | Pirates | PiratesWhite
    deriving Show

data Entry = P Player Shots  Goals  Assists PIM GoalsF GoalsA
           | G Player ShotsA GoalsA Assists PIM
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
    deriving Show

type Number = Int
type Name   = String

--
-- Magic Voodoo Spells
--

main = putStrLn $ renderHtml $ html $ do
    head $ do
        H.title $ "Revolution Sharks Statistics"
        style ! type_ "text/css" $ string $ intercalate "\n"
            [ "table   { margin-top: 0.5em; }"
            , "td, th  { padding: 0 0.5em; }"
            , ".text   { text-align: left; }"
            , ".number { text-align: right }"
            ]
    body $ do
        h1 $ "Revolution Sharks Statistics"
        mapM_ game games

game :: Game -> Html
game (Game rnd opp entries) = do
    h2 $ string $ "Round " ++ show rnd ++ " (vs " ++ show opp ++ ")"
    table $ do
        playerHeader
        playerEntries entries
    table $ do
        goalieHeader
        goalieEntries entries
--
-- Player stats
--

playerHeader :: Html
playerHeader = tr $ do
    ths "#"      "Number"
    ths "Player" "Player"
    thn "G"      "Goals"
    thn "A"      "Assists"
    thn "P"      "Points"
    thn "S"      "Shots"
    thn "S%"     "Shot Percentage"
    thn "+/-"    "Plus/Minus"
    thn "PIM"    "Penalties In Minutes"

playerEntries :: [Entry] -> Html
playerEntries = mapM_ entry
  where
    entry (P p s g a pim gf ga) = tr $ do
        tds (show $ number p)
        tds (name p)
        tdi g
        tdi a
        tdi (g + a)
        tdi s
        tdn $ printf "%.1f" $ shotPct s g
        tdi (gf - ga)
        tdi pim
    entry _ = return ()

--
-- Goalie stats
--

goalieHeader :: Html
goalieHeader = tr $ do
    ths "#"      "Number"
    ths "Goalie" "Goalie"
    thn "SA"     "Shots Against"
    thn "GA"     "Goals Against"
    thn "Sv"     "Saves"
    thn "Sv%"    "Save Percentage"
    thn "A"      "Assists"
    thn "PIM"    "Penalties In Minutes"

goalieEntries :: [Entry] -> Html
goalieEntries = mapM_ entry
  where
    entry (G p sa ga a pim) = tr $ do
        tdi (number p)
        tds (name p)
        tdi sa
        tdi ga
        tdi (sa - ga)
        tdn $ drop 1 $ printf "%.3f" $ savePct sa ga
        tdi a
        tdi pim
    entry _ = return ()

--
-- Helpers
--

shotPct :: Shots -> Goals -> ShotPct
shotPct _ 0 = 0
shotPct s g | s >= g    = 100 * realToFrac g / realToFrac s
            | otherwise = error ("shotPct error: " ++ show g ++ " goals > " ++ show s ++ " shots")

savePct :: ShotsA -> GoalsA -> SavePct
savePct _ 0 = 1
savePct sa ga | sa >= ga  = realToFrac (sa - ga) / realToFrac sa
              | otherwise = error ("savePct error: " ++ show ga ++ " goals against > " ++ show sa ++ " shots against")

ths = th' "text"
thn = th' "number"
th' cls abbrev full = th ! class_ cls ! title full $ abbrev

tdi x = td ! class_ "number" $ int x
tds x = td ! class_ "text"   $ string x
tdn x = td ! class_ "number" $ string x

int :: Int -> Html
int = string . show
