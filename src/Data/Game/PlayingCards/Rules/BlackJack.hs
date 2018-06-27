
module Data.Game.PlayingCards.Rules.BlackJack
  ( Scorable(..)
  , GameOver
  ) where

import Data.List

import Data.Game.PlayingCards.Monad
import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL
import qualified Data.Game.PlayingCards.Table as TB

------------------------------------------------------------------------
directProcutList :: [[a]] -> [[a]]
directProcutList = foldl (\ls vs -> [ v : l | v <- vs, l <- ls ]) [[]]

------------------------------------------------------------------------
instance Scorable PC.Card where
  score' (PC.Card _ v _)
    | v == PC.Ace                 = [11,1]
    | elem v [PC.Jack .. PC.King] = [10]
    | otherwise                   = [1 + fromEnum v]
  score' _                         = error "can not get score"

------------------------------------------------------------------------
-- instance Scorable PC.Deck where
-- https://wiki.haskell.org/List_instance
instance Scorable PC.Deck where
  score (PC.Deck d) =
    let (s, b) = span (<22) $ sort $ map sum $
                 directProcutList $ map score' d
    in if null s
       then head b -- == minimum b -- assume that b is sorted.
       else last s -- == maximum s -- assume that s is sorted.

------------------------------------------------------------------------
instance Scorable PL.Player where
  score (PL.Player _ h) = score h

------------------------------------------------------------------------
-- instance Scorable (String, TB.Table) where
--   score (n, tbl) = 

------------------------------------------------------------------------
-- isBurst :: PL.Player -> Bool
-- isBurst p = 
