
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

    , fullSetCards    

    , isJoker
    
    , flipCard
    , frontCard
    , backCard
    , viewCard
    ) where

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
  } | Joker
  { side :: Side
  } | BackCard
  deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------
fullSetCards :: [Card]
fullSetCards = jokers ++ cards
  where
    jokers = replicate 2 $ Joker Front
    cards = [Card s v Front|s <- [Club .. Spade], v <- [Ace .. King]]

------------------------------------------------------------------------
flipCard
  :: Card
  -> Card
flipCard (Joker Front)   = Joker Back
flipCard (Joker Back)    = Joker Front
flipCard (Card s v Front) = Card s v Back
flipCard (Card s v Back)  = Card s v Front
flipCard BackCard         = error "try to flip BackCard."

------------------------------------------------------------------------
frontCard
  :: Card
  -> Card
frontCard (Joker _)    = Joker Front
frontCard (Card s v _)  = Card s v Front
frontCard BackCard      = error "try to flip BackCard."

------------------------------------------------------------------------
backCard
  :: Card
  -> Card
backCard (Joker _)    = Joker Back
backCard (Card s v _)  = Card s v Back
backCard BackCard      = error "try to flip BackCard."

------------------------------------------------------------------------
viewCard
  :: Card
  -> Card
viewCard (Joker Front)   = Joker Front
viewCard (Joker Back)    = BackCard
viewCard (Card s v Front) = Card s v Front
viewCard (Card _ _ Back)  = BackCard
viewCard BackCard         = BackCard

------------------------------------------------------------------------
isJoker
  :: Card
  -> Bool
isJoker (Joker _) = True
isJoker _         = False
