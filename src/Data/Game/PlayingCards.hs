
module Data.Game.PlayingCards
  ( Suit(..)
  , Value(..)
  , Side(..)
  , Card(..)

  , Deck(..)

  , frontCard
  , backCard
  , viewCard

  , mkfullDeck
  , shuffleDeck
  , draw'
  , drawFront
  , drawBack
  ) where

import Data.List
import System.Random
import Control.Monad.State

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
data Card = Card   { suit  :: Suit
                   , value :: Value
                   , side  :: Side
                   }
          | Jorker { side  :: Side
                   }
          | BackCard
          deriving ( Show, Read, Eq, Ord )

newtype Deck = Deck
  { deck :: [Card]
  } deriving ( Show, Read, Eq, Ord )

------------------------------------------------------------------------
frontCard :: Card -> Card
frontCard (Card s v _) = (Card s v Front)
frontCard (Jorker _)   = (Jorker Front)
frontCard BackCard     = BackCard

------------------------------------------------------------------------
backCard :: Card -> Card
backCard (Card s v _) = (Card s v Back)
backCard (Jorker _)   = (Jorker Back)
backCard BackCard     = BackCard

------------------------------------------------------------------------
-- | view other players card. Back side cards do not show information.
viewCard :: Card -> Card
viewCard c@(Card _ _ Front) = c
viewCard (Card _ _ Back)    = BackCard
viewCard c@(Jorker Front)   = c
viewCard (Jorker Back)      = BackCard
viewCard BackCard           = BackCard

------------------------------------------------------------------------
mkfullDeck
  :: Int    -- ^ number of Jorkers
  -> Deck
mkfullDeck nj = Deck $ cards ++ (replicate nj $ Jorker Back)
  where
    cards = [ Card s v  Back |
              s <- [Club .. Spade], v <- [Ace .. King] ]

------------------------------------------------------------------------
shuffleDeck :: RandomGen g => Deck -> g -> (Deck, g)
shuffleDeck (Deck d) g = ((Deck d'), g')
  where
    l = length d
    (ind, g') = runState (replicateM l $ state $ randomR (1,l)) g
    d' = map fst $ sortOn snd $ zip d ind

------------------------------------------------------------------------
draw' :: (Card -> Card) -> (Card -> Card) -> Deck -> (Card, Deck)
draw' op view (Deck d) = (view $ op $ head d, Deck $ tail d)

------------------------------------------------------------------------
drawFront :: Deck -> (Card, Deck)
drawFront = draw' frontCard viewCard

------------------------------------------------------------------------
drawBack :: (Card -> Card) -> Deck -> (Card, Deck)
drawBack view = draw' backCard view

