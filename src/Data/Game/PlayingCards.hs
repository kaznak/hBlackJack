
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
    , frontCard
    , viewCard

    , makeDeck
    , flipDeck
    , shuffleDeck
    , drawCards
    ) where

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

-- | BackCard is other players hand and used to represent unknown card.
data Card = Card
  { suit  :: Suit
  , value :: Value
  , side  :: Side
  } | Jorker
  { side :: Side
  } | BackCard
  deriving (Show, Read, Eq, Ord)


------------------------------------------------------------------------
makeDeck
  :: Int  -- ^ number of jorkers
  -> [Card]
makeDeck nj = cards ++ jorkers
  where jorkers = take nj $ repeat $ Jorker Front
        cards = [Card s v Front|s <- [Club .. Spade], v <- [Ace .. King]]

------------------------------------------------------------------------
flipCard
  :: Card
  -> Card
flipCard (Jorker Front)   = Jorker Back
flipCard (Jorker Back)    = Jorker Front
flipCard (Card s v Front) = Card s v Back
flipCard (Card s v Back)  = Card s v Front
flipCard BackCard         = error "try to flip BackCard."

------------------------------------------------------------------------
frontCard
  :: Card
  -> Card
frontCard (Jorker _)    = Jorker Front
frontCard (Card s v _)  = Card s v Front
frontCard BackCard      = error "try to flip BackCard."

------------------------------------------------------------------------
viewCard
  :: Card
  -> Card
viewCard (Jorker Front)   = Jorker Back
viewCard (Jorker Back)    = BackCard
viewCard (Card s v Front) = Card s v Back
viewCard (Card _ _ Back)  = BackCard
viewCard BackCard         = BackCard

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
drawCards
  :: [Card]           -- ^ Deck
  -> Int              -- ^ Number of cards
  -> ([Card], [Card]) -- ^ Took cards and Rest cards
drawCards d n = (take n d, drop n d)
