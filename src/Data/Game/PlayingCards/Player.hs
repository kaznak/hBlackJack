
module Data.Game.PlayingCards.Player
  ( Player(..)

  , PlayerS
  
  , viewPlayer
  , addHand
  , openHand

  , findPlayerS
  , addHandS  
  ) where

import Data.List

import qualified Data.Game.PlayingCards as PC

------------------------------------------------------------------------
data Player = Player
  { name :: String
  , hand :: PC.Deck
  } deriving
  ( Show
  )

type PlayerS = [Player]

------------------------------------------------------------------------
viewPlayer :: Player -> Player
viewPlayer (Player n (PC.Deck h)) = Player n $ PC.Deck $ map PC.viewCard h

------------------------------------------------------------------------
addHand :: PC.Card -> Player -> Player
addHand c (Player n (PC.Deck h)) = Player n $ PC.Deck $ c:h

------------------------------------------------------------------------
openHand :: Player -> Player
openHand (Player n (PC.Deck h)) = Player n $ PC.Deck $ map PC.frontCard h

------------------------------------------------------------------------
------------------------------------------------------------------------
updateList :: Int -> a -> [a] -> [a]
updateList i v' l = left ++ [v'] ++ (tail right)
  where
    (left,right) = splitAt i l

------------------------------------------------------------------------
findPlayerS :: String -> PlayerS -> Maybe (Int, Player)
findPlayerS n ps = do
  ind <- findIndex ((n ==) . name) ps
  -- assert n == (name $ ps!!ind)
  return (ind, ps !! ind)

------------------------------------------------------------------------
addHandS :: PC.Card -> String -> PlayerS -> Maybe PlayerS
addHandS c n ps = do
  (i, p) <- findPlayerS n ps
  return $ updateList i (addHand c p) ps
