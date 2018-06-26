
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.List
import Data.Maybe

import System.Random

import Control.Monad.IO.Class
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
fullSetCards = [ Card s v  Back |
                 s <- [Club .. Spade], v <- [Ace .. King] ]

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
toScore' :: Card -> [Int]
toScore' (Card _ v _)
  | v == Ace              = [1,11]
  | elem v [Jack .. King] = [10]
  | otherwise             = [1 + fromEnum v]
toScore' _                = error "can not get score"

------------------------------------------------------------------------
directProcutList :: [[a]] -> [[a]]
directProcutList = foldl (\ls vs -> [ v : l | v <- vs, l <- ls ]) [[]]

toScore :: [Card] -> Int
toScore cs =
  let (s, b) = span (<22) $ sort $ map sum $
               directProcutList $ map toScore' cs
  in if null s
     then head b -- == minimum b -- assume that b is sorted.
     else last s -- == maximum s -- assume that s is sorted.

------------------------------------------------------------------------
------------------------------------------------------------------------
data Player = Player
  { name :: String
  , hand :: [Card]
  } deriving
  ( Show
  )

------------------------------------------------------------------------
viewPlayer :: Player -> Player
viewPlayer (Player n h) = Player n (map viewCard h)

------------------------------------------------------------------------
addHand :: Card -> Player -> Player
addHand c (Player n h) = Player n (c:h)

------------------------------------------------------------------------
------------------------------------------------------------------------
data Table = Table
  { deck   :: [Card]
  , player :: [Player]
  } deriving
  ( Show
  )

------------------------------------------------------------------------
updateList :: (Int, a) -> [a] -> [a]
updateList (i, v') l = left ++ [v'] ++ (tail right)
  where
    (left,right) = splitAt i l

------------------------------------------------------------------------
------------------------------------------------------------------------
data GameState = GameState
  { gen :: StdGen
  , tbl :: Table
  } deriving
  ( Show
  )

newtype Game a = Game
  { runGame :: StateT GameState IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState GameState
  , MonadIO
  )

------------------------------------------------------------------------
findPlayer :: String -> GameState -> Maybe (Int, Player)
findPlayer n (GameState _ (Table _ ps)) = do
  ind <- findIndex ((n ==) . name) ps
  -- assert n == (name $ ps!!ind)
  return (ind, ps!!ind)

------------------------------------------------------------------------
initGame' :: Int -> GameState
initGame' seed = GameState (mkStdGen seed) $
  Table fullSetCards [ (Player "Dealer" [])
                     , (Player "Player" []) ]

initGame :: Int -> Game ()
initGame = put . initGame'

------------------------------------------------------------------------
shuffleDeck' :: GameState -> GameState
shuffleDeck' (GameState g (Table d p)) = GameState g' $ Table d' p
  where
    l = length d
    (ind, g') = runState (replicateM l $ state $ randomR (1,l)) g
    d' = map fst $ sortOn snd $ zip d ind

shuffleDeck :: Game ()
shuffleDeck = modify shuffleDeck'

------------------------------------------------------------------------
draw' :: (Card -> Card) -> (Card -> Card) -> String -> GameState -> (Card, GameState)
draw' op view pn gs@(GameState g (Table d ps)) =
  (c'', GameState g (Table d' ps'))
  where
    ([c], d') = splitAt 1 d
    c' = op c
    (i, p) = fromJust $ findPlayer pn gs
    ps' = updateList (i,addHand c' p) ps
    c'' = view c'

drawFront' :: String -> GameState -> (Card, GameState)
drawFront' = draw' frontCard viewCard

drawFront :: String -> Game Card
drawFront = state . drawFront'

drawSelf' :: String -> GameState -> (Card, GameState)
drawSelf' = draw' id id

drawSelf :: String -> Game Card
drawSelf = state . drawSelf'

drawView' :: String -> GameState -> (Card, GameState)
drawView' = draw' id viewCard

drawView :: String -> Game Card
drawView = state . drawView'

------------------------------------------------------------------------
viewPlayerGS :: String -> (Player -> Player) -> GameState -> IO ()
viewPlayerGS n view gs = putStrLn $ show $ view $ snd $ fromJust $ findPlayer n gs

findPlayerGS :: String -> GameState -> Game Player
findPlayerGS n gs = return $ snd $ fromJust $ findPlayer n gs

------------------------------------------------------------------------
------------------------------------------------------------------------
checkStat :: Player -> Game Bool
checkStat (Player n h) = do
  score <- return $ toScore h
  if 22 > score
    then do liftIO $ putStrLn $ n ++ " Score: " ++ (show score)
            return True
    else do liftIO $ putStrLn $ n ++ " Score: " ++ (show score) ++ " Burst!"
            return False

------------------------------------------------------------------------
type Action = Player -> Game Bool

------------------------------------------------------------------------
dealerAction :: Action
dealerAction (Player n h) = do
  score <- return $ toScore h
  if 17 > score
    then do c <- drawFront n
            liftIO $ putStrLn $ n ++ " draw " ++ (show c)
            return True
    else do return False

------------------------------------------------------------------------
playerAction :: Action
playerAction p@(Player n _) = do
  liftIO $ putStrLn $ n ++ ": Hit or Stand?[H/S]"
  cmd <- return . toUpper . head =<< liftIO getLine
  case cmd of
    'S' -> return False
    'H' -> do c <- drawSelf n
              liftIO $ putStrLn $ n ++ " draw " ++ (show c)
              return True
    _   -> do liftIO $ putStrLn "Bad Command : Hit or Stand?[H/S]"
              playerAction p

------------------------------------------------------------------------
turn :: (Player -> Game Bool) -> String -> Game ()
turn action n = do
  gs <- get
  p <- findPlayerGS n gs
  liftIO $ putStrLn $ show p
  ch <- checkStat p
  if ch
    then do
    continueP <- action p
    if continueP
      then do turn action n
      else return ()
    else return ()

------------------------------------------------------------------------
gameExec :: Game ()
gameExec = do
  shuffleDeck ;
  let n = "Player"
    in do c <- drawFront n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c)
          c <- drawSelf  n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c)
  let n = "Dealer"
    in do c <- drawFront n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c)
          c <- drawView  n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c)
  ----------------------------------------------------------------------
  turn playerAction "Player"
  turn dealerAction "Dealer"
  ----------------------------------------------------------------------
  return ()

------------------------------------------------------------------------
game :: Int -> IO ()
game seed = runStateT (runGame gameExec) (initGame' seed) >> return ()
