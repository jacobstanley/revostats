{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Html (renderHtmlStats) where

import           Data.List hiding (head, span)
import qualified Prelude as P
import           Prelude hiding (head, id, div, span)
import qualified Text.Blaze.Html4.Strict as H
import           Text.Blaze.Html4.Strict hiding (map, title)
import           Text.Blaze.Html4.Strict.Attributes hiding (name, span, style)
import           Text.Blaze.Renderer.String (renderHtml)
import           Text.Printf

import           Floorball hiding ((-))


renderHtmlStats :: [Game] -> String
renderHtmlStats games = renderHtml $ docTypeHtml $ do
    head $ do
        H.title "Revolution Sharks Statistics"
        style ! type_ "text/css" $ string $ "\n" ++ intercalate "\n" css ++ "\n"
    body $ do
        h1 "Revolution Sharks Statistics"
        h2 "Games"
        mapM_ game games
        h2 "Players"
        pre $ string $ show xs
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


-- Player stats

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


-- Goalie stats

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


-- Helpers

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
