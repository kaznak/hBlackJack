
-- | This module defines standard modern playing cards with jokers
-- and provide simple deck making function.
-- If you want to shuffle a deck,
-- [random-shuffle](https://hackage.haskell.org/package/random-shuffle)
-- is useful.

module Data.Game.PlayingCards
    ( Card(..)
    , Suit(..)
    , Value(..)
    , Side(..)

    , flipCard

    , makeDeck
    , flipDeck
    , shuffleDeck
    , drawCard
    ) where

import Development.Placeholders

import System.Random.Shuffle

------------------------------------------------------------------------
-- | Card type definition.

data Suit = Club | Diamond | Heart | Spade
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Value = Ace  | Two   | Three | Four | Five
           | Six  | Seven | Eight | Nine | Ten
           | Jack | Queen | King
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Side = Back | Front
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | !FUTER WORKS! To apply this Card type to agent system,
-- uniq id is require for each Card and
-- it's value must be designate independently
-- from the card Suit and Value.
data Card = Card
  { suit  :: Suit
  , value :: Value
  , side  :: Side
  } | Jorker
  { side :: Side
  } deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------
makeDeck
  :: Int  -- ^ number of jorkers
  -> [Card]
makeDeck nj = cards ++ jorkers
  where jorkers = take nj $ repeat $ Jorker Front
        cards = [ Card suit value Front |
                  suit <- [Club .. Spade],
                  value <- [Ace .. King] ]

------------------------------------------------------------------------
flipCard
  :: Card
  -> Card
flipCard (Jorker Front)   = Jorker Back
flipCard (Jorker Back)    = Jorker Front
flipCard (Card s v Front) = Card s v Back
flipCard (Card s v Back)  = Card s v Front

------------------------------------------------------------------------
flipDeck
  :: [Card]
  -> [Card]
flipDeck deck = reverse $ map flipCard deck

------------------------------------------------------------------------
shuffleDeck
  :: [Card] -- ^ Deck (c1, c2 .. cn)
  -> [Int]  -- ^ Sample Number (r1, r2 .. r(n-1))
  -> [Card]    
shuffleDeck deck r = shuffle deck r

------------------------------------------------------------------------
drawCard
  :: [Card]           -- ^ Deck
  -> Int              -- ^ Number of cards
  -> ([Card], [Card]) -- ^ Took cards and Rest cards
drawCard d n = (take n d, drop n d)
