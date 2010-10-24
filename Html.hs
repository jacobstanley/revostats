{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Html (renderHtmlStats) where

import           Control.Monad (forM_)
import           Data.List hiding (head, span)
import qualified Prelude as P
import           Prelude hiding (head, id, div, span)
import qualified Text.Blaze.Html4.Strict as H
import           Text.Blaze.Html4.Strict hiding (map, title)
import qualified Text.Blaze.Html4.Strict.Attributes as A
import           Text.Blaze.Html4.Strict.Attributes hiding (name, span, style)
import           Text.Blaze.Renderer.String (renderHtml)
import           Text.Printf

import           Floorball hiding ((-))


------------------------------------------------------------------------

renderHtmlStats :: [GameStats] -> String
renderHtmlStats games = renderHtml $ docTypeHtml $ do
    head $ do
        H.title "Revolution Sharks Statistics"
        style ! type_ "text/css" $ string $ "\n" ++ intercalate "\n" css ++ "\n"
    body $ do
        h1 "Revolution Sharks Statistics"
        h2 "Games"
        mapM_ renderGame games
        h2 "Players"
        mapM_ renderScorer (scoringStats games)
        h2 "Goalies"
        mapM_ renderGoalie (goalieStats games)
  where
    css = [ "table   { margin-top: 0.5em; }"
          , "td, th  { padding: 0 0.5em; }"
          , ".text   { text-align: left; }"
          , ".number { text-align: right; }"
          ]


renderGame :: GameStats -> Html
renderGame (GameStats (Game rnd opp) ps gs) = do
    h3 $ string $ "Round " ++ show rnd ++ " (vs " ++ show opp ++ ")"
    table $ do
        gameHeaders scoringHeaders "Player"
        gameRows scoringCells ps
    table $ do
        gameHeaders goalieHeaders "Goalie"
        gameRows goalieCells gs


renderScorer :: PlayerStats ScoringStat -> Html
renderScorer (PlayerStats (Player num name) gs) = do
    h3 ! id (stringValue $ show num)
       $ string $ "#" ++ show num ++ " " ++ name
    table $ do
        individualHeaders scoringHeaders
        individualRows scoringCells gs


renderGoalie :: PlayerStats GoalieStat -> Html
renderGoalie (PlayerStats (Player num name) gs) = do
    h3 ! id (stringValue $ show num)
       $ string $ "#" ++ show num ++ " " ++ name
    table $ do
        individualHeaders goalieHeaders
        individualRows goalieCells gs


------------------------------------------------------------------------
-- Common player stats

gameHeaders :: Html -> String -> Html
gameHeaders otherHeaders name = tr $ do
    ths "#" "Number"
    ths (string name) (stringValue name)
    otherHeaders

gameRows :: (a -> Html) -> [(Player, a)] -> Html
gameRows otherCells = mapM_ $ \(Player num name, stat) -> tr $ do
    tds (show num)
    tda ("#" ++ show num) name
    otherCells stat

individualHeaders :: Html -> Html
individualHeaders otherHeaders = tr $ do
    ths "Rd"  "Round"
    ths "Opp" "Opponent"
    otherHeaders

individualRows :: Total a => (a -> Html) -> [(Game, a)] -> Html
individualRows otherCells games = do
    forM_ games $ \(Game rnd opp, stat) -> tr $ do
        tds (show rnd)
        tds (show opp)
        otherCells stat
    tr $ do
        th ! colspan "2" ! class_ "text" $ "Total"
        otherCells (total $ map snd games)

------------------------------------------------------------------------
-- Scoring stats

scoringHeaders :: Html
scoringHeaders = do
    thn "G"   "Goals"
    thn "A"   "Assists"
    thn "P"   "Points"
    thn "S"   "Shots"
    thn "S%"  "Shot Percentage"
    thn "+/-" "Plus/Minus"
    thn "PIM" "Penalties In Minutes"

scoringCells :: ScoringStat -> Html
scoringCells (P s g a pim gf ga) = do
    tdi g
    tdi a
    tdi (g + a)
    tdi s
    tdn $ printf "%.1f" $ shotPct s g
    tdi (gf - ga)
    tdi pim


------------------------------------------------------------------------
-- Goalie stats

goalieHeaders :: Html
goalieHeaders = do
    thn "SA"  "Shots Against"
    thn "GA"  "Goals Against"
    thn "Sv"  "Saves"
    thn "Sv%" "Save Percentage"
    thn "A"   "Assists"
    thn "PIM" "Penalties In Minutes"

goalieCells :: GoalieStat -> Html
goalieCells (G sa ga a pim) = do
    tdi sa
    tdi ga
    tdi (sa - ga)
    tdn $ drop 1 $ printf "%.3f" $ savePct sa ga
    tdi a
    tdi pim


------------------------------------------------------------------------
-- Helpers

ths = th' "text"
thn = th' "number"
th' cls abbrev full = th ! class_ cls ! title full $ abbrev

tdi x = td ! class_ "number" $ int x
tds x = td ! class_ "text"   $ string x
tdn x = td ! class_ "number" $ string x

tda url x = td ! class_ "text" $ a ! href (stringValue url) $ string x

int :: Int -> Html
int = string . show
