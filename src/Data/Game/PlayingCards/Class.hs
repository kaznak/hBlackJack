
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Game.PlayingCards.Class
  ( Viewable(..)
  , Scoreable'(..)
  , Scoreable(..)

  , GamePhase(..)
  ) where

import Data.List
import Data.Maybe

import System.Random

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except

------------------------------------------------------------------------
class Viewable a where
  view :: a -> a

------------------------------------------------------------------------
class Scoreable' a where
  score' :: a -> [Int]

------------------------------------------------------------------------
class Scoreable' a => Scoreable a where
  score :: a -> Int
  score cs = head $ score' $ cs

------------------------------------------------------------------------
data GamePhase = Playing
               | GameOver
               deriving ( Show, Read, Eq )

------------------------------------------------------------------------
