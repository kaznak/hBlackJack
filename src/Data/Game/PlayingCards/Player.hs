
-- | Simple BlackJack

module Data.Game.PlayingCards.Player
    ( Player(..)

    , makePlayer
    
    , addHand
    , dropHand
    , flipHand
    ) where

import Development.Placeholders

import qualified Data.Game.PlayingCards as PC
import Data.List

import System.Random
import System.Random.Shuffle

import Control.Monad.State.Lazy

------------------------------------------------------------------------
data Player = Player
  { name :: String    -- ^ Name of Player, which work as ID.
  , hand :: [PC.Card] -- ^ Hand of Player.
  } deriving Show

------------------------------------------------------------------------
makePlayer
  :: String -- ^ player name
  -> Player
makePlayer name = Player name []

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
dropHand (Player n h) = Player n []

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
