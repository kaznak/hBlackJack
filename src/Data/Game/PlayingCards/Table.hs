
module Data.Game.PlayingCards.Table
  ( Table(..)

  , draw'
  , drawFront
  , drawBack
  ) where

import Data.List
import Data.Maybe

import qualified Data.Game.PlayingCards as PC
import qualified Data.Game.PlayingCards.Player as PL

------------------------------------------------------------------------
data Table = Table
  { deck   :: PC.Deck
  , players :: PL.PlayerS
  } deriving
  ( Show
  )

------------------------------------------------------------------------
-- findPlayer 

------------------------------------------------------------------------
draw'
  :: (PC.Deck -> (PC.Card, PC.Deck))
  -> String
  -> Table
  -> Maybe (PC.Card, Table)
draw' draw n (Table d ps) = do
  (c, d') <- return $ draw d
  psm <- PL.addHandS c n ps
  return (c, Table d' psm)

------------------------------------------------------------------------
drawFront :: String -> Table -> Maybe (PC.Card, Table)
drawFront = draw' PC.drawFront

------------------------------------------------------------------------
drawBack
  :: (PC.Card -> PC.Card)
  -> String
  -> Table
  -> Maybe (PC.Card, Table)
drawBack = draw' . PC.drawBack

