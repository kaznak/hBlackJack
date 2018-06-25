
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
-- import Data.Functor
import Data.IORef

import System.Random
import System.Random.Shuffle

-- import Control.Monad.Error

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
type Deck = IORef [Card]
------------------------------------------------------------------------
deck :: IO Deck
deck = shuffleM fullSetCards >>= newIORef

------------------------------------------------------------------------
type Player = IORef (String, [Card])

dealer :: IO Player
dealer = newIORef ("Dealer", [])

player :: IO Player
player = newIORef ("Player", [])

------------------------------------------------------------------------
draw' :: (Card -> Card) -> Player -> IO ()
draw' op player = do
  d <- deck
  cs <- readIORef d
  card <- return $ op $ head cs
  (name, hand) <- readIORef player
  writeIORef d $ tail cs
  writeIORef player (name, card:hand)
  putStrLn $ name ++ " draw " ++ (show card)

drawFront :: Player -> IO()
drawFront = draw' frontCard

draw :: Player -> IO()
draw = draw' id
  
------------------------------------------------------------------------
checkStat :: ([Card] -> [Card]) -> Player -> IO Bool
checkStat view player = do
  (name, hand) <- readIORef player
  score <- return $ sum $ map toScore hand
  putStrLn $ ((name ++) ": " ++ ) $ show $ view hand
  putStrLn $ ((name ++) ": " ++ ) $ show score
  if score > 21
    then putStrLn (name ++ ": Burst") >> return False
    else return True

------------------------------------------------------------------------
type Check = Player -> IO Bool
type Action = Player -> IO Bool

------------------------------------------------------------------------
------------------------------------------------------------------------
turn :: Check -> Action -> Player -> IO ()
turn check action player = do
  ch <- check player
  if ch
    then do
    continueP <- action player
    if continueP
      then do turn check action player
      else return ()
    else return ()

------------------------------------------------------------------------
dealerAction :: Action
dealerAction player = do
  (_, hand) <- readIORef player
  score <- return $ sum $ map toScore hand
  if 17 > score
    then draw player >> return True
    else return False

------------------------------------------------------------------------
playerAction :: Action
playerAction player = do
  (name, _) <- readIORef player
  putStrLn $ name ++ ": Hit or Stand?[H/S]"
  cmd <- getLine >>= return . toUpper . head
  case cmd of
    'S' -> return False
    'H' -> draw player >> return True
    _   -> putStrLn "Bad Command : Hit or Stand?[H/S]"
           >> playerAction player

------------------------------------------------------------------------
game :: Int -> IO ()
game seed = do
  setStdGen $ mkStdGen seed
  p <- player ; drawFront p ; draw p
  d <- dealer ; drawFront d ; draw d
  _ <- checkStat (map id) p
  _ <- checkStat (map viewCard) d
  ----------------------------------------------------------------------
  turn (checkStat $ map id) playerAction p
  turn (checkStat $ map id) dealerAction d

