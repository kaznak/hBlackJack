
-- | module for BlackJack rules

module Data.Game.PlayingCards.Rules.BlackJack
  ( GameState(..)
  , checkScore
  , getWinner
  ) where

import Data.List
import Data.Maybe

import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL
import qualified Data.Game.PlayingCards.Table as TB

------------------------------------------------------------------------
data GameState = GameState
  { winner :: [PL.Player]
  } deriving(Show)

------------------------------------------------------------------------
cardScore
  :: PC.Card
  -> Int
cardScore (PC.Card _ v _) | s < 10 = s + 1
  where s = fromEnum v
cardScore (PC.Card _ _ _) = 10
cardScore _  = error "bad card."

------------------------------------------------------------------------
checkScore
  :: PL.Player -- ^ player state
  -> Int
checkScore = sum . map cardScore . PL.hand

------------------------------------------------------------------------
getWinner
  :: TB.Table -- ^ table
  -> [PL.Player]
getWinner (TB.Table _ _ _ ps) =
  if (psc > 21)
  then [d]
  else if dsc > 21
  then [p]
  else if (dsc > psc)
  then [d]
  else [p]
  where
    d = fromJust $ ps !! 0
    dsc = checkScore d
    p = fromJust $ ps !! 1
    psc = checkScore p
