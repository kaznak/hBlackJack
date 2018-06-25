
module BlackJack
  ( Suit(..)
  , Value(..)
  , Side(..)
  , Card(..)

  , fullSetCards

  , frontCard
  , viewCard

------------------------------------------------------------------------
  , game
  ) where

import Data.Char
import Data.Functor
import Data.IORef

import System.Random
import System.Random.Shuffle

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
data Card = Card
  { suit  :: Suit
  , value :: Value
  , side  :: Side
  } | BackCard
  deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------
fullSetCards :: [Card]
fullSetCards = [ Card s v  Back | s <- [Club .. Spade], v <- [Ace .. King] ]

------------------------------------------------------------------------
frontCard :: Card -> Card
frontCard (Card s v _) = (Card s v Front)
frontCard BackCard = error "BackCard"

------------------------------------------------------------------------
viewCard :: Card -> Card
viewCard c@(Card _ _ Front) = c
viewCard (Card _ _ Back) = BackCard
viewCard BackCard = BackCard

------------------------------------------------------------------------
toScore :: Card -> Int
toScore (Card _ v _) = score
  where
    s = 1 + fromEnum v
    score = if s < 10
            then s
            else 10

------------------------------------------------------------------------
------------------------------------------------------------------------
drawCard :: IORef [Card] -> IORef [Card] -> IO Card
drawCard d p = do
  cs <- readIORef d
  writeIORef d $ tail cs
  readIORef p >>= writeIORef p . (head cs:)
  return (head cs)

------------------------------------------------------------------------
printScore :: String -> ([Card] -> [Card]) -> IORef [Card] -> IO ()
printScore name view p = do
  hs <- readIORef p
  putStrLn $ ((name ++) ": " ++ ) $ show $ view hs
  putStrLn $ ((name ++) ": " ++ ) $ show $ sum $ map toScore hs
  
------------------------------------------------------------------------
------------------------------------------------------------------------
turn
  :: IORef [Card]
  -> (IORef [Card] -> IO ())
  -> (IORef [Card] -> IO Bool)
  -> IO ()
turn player printer action = do
  printer player
  continueP <- action player
  if continueP
    then do turn player printer action
    else return ()
------------------------------------------------------------------------
dealerAction :: (IORef [Card] -> IO Card) -> IORef [Card] -> IO Bool
dealerAction draw player = do
  score <- readIORef player >>= return . sum . map toScore
  if 17 > score
    then draw player >>= putStrLn . ("Draw: " ++ ) . show >> return True
    else return False

------------------------------------------------------------------------
playerAction :: (IORef [Card] -> IO Card) -> IORef [Card] -> IO Bool
playerAction draw player = do
  s <- readIORef player >>= return . sum . map toScore
  if 21 < s
    then do
    putStrLn "Player : Burst"
    return False
    else do
    putStrLn "Player : Hit or Stand?[H/S]"
    l <- getLine >>= return . toUpper . head
    case l of
      'H' -> do draw player >>= putStrLn . ("Draw: " ++ ) . show >> return True
      'S' -> return False

------------------------------------------------------------------------
game :: Int -> IO ()
game seed = do
  setStdGen $ mkStdGen seed
  draw <- (shuffleM fullSetCards >>= newIORef >>= return . drawCard)
  player <- newIORef [] ; dealer <- newIORef []
  ----------------------------------------------------------------------
  _ <- draw player ; modifyIORef player (map frontCard) ; _ <- draw player
  printScore "Player" id player
  _ <- draw dealer ; modifyIORef dealer (map frontCard) ; _ <- draw dealer
  printScore "Dealer" (map viewCard) dealer
  ----------------------------------------------------------------------
  turn player (printScore "Player" id) (playerAction draw)
  turn dealer (printScore "Dealer" $ map viewCard) (dealerAction draw)
  ----------------------------------------------------------------------
