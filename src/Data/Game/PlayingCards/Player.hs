
module Data.Game.PlayingCards.Player
    ( Player(..)

    , makePlayer
    
    , addHand
    , dropHand
    , flipHand

    , viewPlayer
    ) where

import qualified Data.Game.PlayingCards as PC

------------------------------------------------------------------------
data Player = Player
  { name :: String    -- ^ Name of Player, which work as ID.
  , hand :: [PC.Card] -- ^ Hand of Player.
  } deriving Show

------------------------------------------------------------------------
makePlayer
  :: String -- ^ player name
  -> Player
makePlayer n = Player n []

------------------------------------------------------------------------
addHand
  :: [PC.Card] -- ^ add cards to hand
  -> Player    -- ^ previous player state
  -> Player    -- ^ next player state
addHand cs (Player n h) = Player n (h ++ cs)

------------------------------------------------------------------------
dropHand
  :: Player    -- ^ previous player state
  -> Player    -- ^ next player state
dropHand (Player n _) = Player n []

------------------------------------------------------------------------
flipHand
  :: Int       -- ^ Index of hand that to be flipped
  -> Player    -- ^ previous player state
  -> Player    -- ^ next player state
flipHand ind (Player n h) = Player n h'
  where
    h' = p ++ [(PC.flipCard $ h !! ind)] ++ r
    p  = take ind h
    r  = drop (ind + 1) h

------------------------------------------------------------------------
viewPlayer
  :: Player    -- ^ previous player state
  -> Player    -- ^ next player state
viewPlayer (Player n h) = Player n h'
  where
    h' = map PC.viewCard h
