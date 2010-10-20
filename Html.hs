{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Html (renderHtmlStats) where

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
        gameScoringHeader
        gameScoringRows ps
    table $ do
        gameGoalieHeader
        gameGoalieRows gs


renderScorer :: PlayerStats ScoringStat -> Html
renderScorer (PlayerStats (Player num name) gs) = do
    h3 ! id (stringValue $ show num)
       $ string $ "#" ++ show num ++ " " ++ name
    table $ do
        playerScoringHeader
        playerScoringRows gs


renderGoalie :: PlayerStats GoalieStat -> Html
renderGoalie (PlayerStats (Player num name) gs) = do
    h3 ! id (stringValue $ show num)
       $ string $ "#" ++ show num ++ " " ++ name
    table $ do
        playerGoalieHeader
        playerGoalieRows gs


-- Scoring stats

gameScoringHeader :: Html
gameScoringHeader = tr $ do
    ths "#"      "Number"
    ths "Player" "Player"
    scoringHeader

gameScoringRows :: [(Player, ScoringStat)] -> Html
gameScoringRows = mapM_ $ \(Player num name, stat) -> tr $ do
    tds (show num)
    tda ("#" ++ show num) name
    scoringCells stat

playerScoringHeader :: Html
playerScoringHeader = tr $ do
    ths "Rd"  "Round"
    ths "Opp" "Opponent"
    scoringHeader

playerScoringRows :: [(Game, ScoringStat)] -> Html
playerScoringRows = mapM_ $ \(Game rnd opp, stat) -> tr $ do
    tds (show rnd)
    tds (show opp)
    scoringCells stat

scoringHeader :: Html
scoringHeader = do
    thn "G"      "Goals"
    thn "A"      "Assists"
    thn "P"      "Points"
    thn "S"      "Shots"
    thn "S%"     "Shot Percentage"
    thn "+/-"    "Plus/Minus"
    thn "PIM"    "Penalties In Minutes"

scoringCells :: ScoringStat -> Html
scoringCells (P s g a pim gf ga) = do
    tdi g
    tdi a
    tdi (g + a)
    tdi s
    tdn $ printf "%.1f" $ shotPct s g
    tdi (gf - ga)
    tdi pim


-- Goalie stats

gameGoalieHeader :: Html
gameGoalieHeader = tr $ do
    ths "#"      "Number"
    ths "Goalie" "Goalie"
    goalieHeader

gameGoalieRows :: [(Player, GoalieStat)] -> Html
gameGoalieRows = mapM_ $ \(Player num name, stat) -> tr $ do
    tds (show num)
    tda ("#" ++ show num) name
    goalieCells stat

playerGoalieHeader :: Html
playerGoalieHeader = tr $ do
    ths "Rd"  "Round"
    ths "Opp" "Opponent"
    goalieHeader

playerGoalieRows :: [(Game, GoalieStat)] -> Html
playerGoalieRows = mapM_ $ \(Game rnd opp, stat) -> tr $ do
    tds (show rnd)
    tds (show opp)
    goalieCells stat

goalieHeader :: Html
goalieHeader = do
    thn "SA"     "Shots Against"
    thn "GA"     "Goals Against"
    thn "Sv"     "Saves"
    thn "Sv%"    "Save Percentage"
    thn "A"      "Assists"
    thn "PIM"    "Penalties In Minutes"

goalieCells :: GoalieStat -> Html
goalieCells (G sa ga a pim) = do
    tdi sa
    tdi ga
    tdi (sa - ga)
    tdn $ drop 1 $ printf "%.3f" $ savePct sa ga
    tdi a
    tdi pim


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
