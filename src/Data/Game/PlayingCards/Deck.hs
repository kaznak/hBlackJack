
-- | This module defines standard modern playing cards with jokers
-- and provide simple deck making function.
-- If you want to shuffle a deck,
-- [random-shuffle](https://hackage.haskell.org/package/random-shuffle)
-- is useful.

module Data.Game.PlayingCards.Deck
    ( Deck(..)
    , makeEmptyDeck
    , flipDeck
    , shuffleDeck
    , drawCards

    , newDeckWithoutJoker
    ) where


import qualified Data.Game.PlayingCards as PC

import System.Random
import System.Random.Shuffle

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Random

------------------------------------------------------------------------
data Deck = Deck
  { deck :: [PC.Card]
  , gen  :: StdGen
  } deriving (Show)

------------------------------------------------------------------------
makeEmptyDeck
  :: StdGen  -- ^ random number generator for shuffling this deck
  -> Deck
makeEmptyDeck g = Deck [] g

------------------------------------------------------------------------
addCards
  :: [PC.Card]
  -> Deck
  -> Deck
addCards cs (Deck d g) = Deck (cs ++ d) g

------------------------------------------------------------------------
removeCardsCond
  :: (PC.Card -> Bool)
  -> Deck
  -> Deck
removeCardsCond cond (Deck d g) = Deck d' g
  where
    d' = filter (not . cond) d

------------------------------------------------------------------------
addFullSetCards
  :: Deck
  -> Deck
addFullSetCards d = addCards PC.fullSetCards d

------------------------------------------------------------------------
newDeckWithoutJoker
  :: StdGen -- ^ random number generator for shuffling this deck
  -> Deck
newDeckWithoutJoker g = removeCardsCond PC.isJoker
                        $ addFullSetCards
                        $ makeEmptyDeck g

------------------------------------------------------------------------
operateCards
  :: ([PC.Card] -> [PC.Card])
  -> Deck
  -> Deck
operateCards op (Deck d g) = Deck (op d) g

------------------------------------------------------------------------
flipDeck
  :: Deck
  -> Deck
flipDeck (Deck d g) = Deck d' g
  where 
    d' = map PC.flipCard $ reverse d

------------------------------------------------------------------------
shuffleDeck
  :: Deck
  -> Deck
shuffleDeck (Deck d g) = Deck d' g'
  where
    d' = undefined
    g' = undefined
    
------------------------------------------------------------------------
drawCards
  :: Int              -- ^ Number of cards
  -> [PC.Card]           -- ^ Deck
  -> ([PC.Card], [PC.Card]) -- ^ Took cards and Rest cards
drawCards = splitAt

------------------------------------------------------------------------
f :: State StdGen [Int]
f = do
  put $ mkStdGen 0
  v1 <- state $ randomR (1, 6)
  v2 <- state $ randomR (1, 6)
  v3 <- state $ randomR (1, 6)
  return [v1, v2, v3]

{-
runState f (mkStdGen 0)
runState f (mkStdGen 1)
runRand (shuffleM PC.fullSetCards >>= return . head) (mkStdGen 0)
-}
