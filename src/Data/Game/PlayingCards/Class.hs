
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data.Game.PlayingCards.Class
  ( Scoreable'(..)
  , Scoreable(..)

  , Player(..)
  , PlayerS(..)

  , GameState(..)
  , Game(..)
  ) where

import System.Random

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except

------------------------------------------------------------------------
class Scoreable' a where
  score' :: a -> [Int]

------------------------------------------------------------------------
class Scoreable' a => Scoreable a where
  score :: a -> Int
  score = head . score'

------------------------------------------------------------------------
class (Eq a, Scoreable a) => Player a where
  name   :: a -> String

------------------------------------------------------------------------
class (Scoreable' a) => PlayerS a where
  player       :: Player b => a -> [b]
  findPlayer   :: Player b => String -> a -> Maybe b
  updatePlayer :: Player b => b -> Maybe a

------------------------------------------------------------------------
data PlayerResult = Win
                  | Lose
                  | Draw
                  deriving ( Show, Read, Eq, Ord, Enum, Bounded )

------------------------------------------------------------------------
data GamePhase = Playing
               | GameOver
               deriving ( Show, Read, Eq )

------------------------------------------------------------------------
class GameState a where
  gen     :: RandomGen g => a -> g
  players :: PlayerS p => a -> p

------------------------------------------------------------------------
data Game a b = Game
  { runGame :: ExceptT GamePhase (StateT a IO) b
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState a
  , MonadIO
  , MonadError GamePhase
  )
