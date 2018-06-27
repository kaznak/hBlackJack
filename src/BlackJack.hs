
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BlackJack
  ( Suit(..)
  , Value(..)
  , Side(..)
  , Card(..)

--   , fullSetCards

--   , frontCard
--   , viewCard

-- ------------------------------------------------------------------------
--   , game
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Either

import System.Random

import Data.Game.PlayingCards.Class
  
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except

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
frontCard :: Card -> Card
frontCard (Card s v _) = (Card s v Front)
frontCard BackCard = error "BackCard"

------------------------------------------------------------------------
instance Viewable Card where
  view c@(Card _ _ Front) = c
  view _                  = BackCard

------------------------------------------------------------------------
instance Scoreable' Card where
  score' (Card _ v _)
    | v == Ace              = [1,11]
    | elem v [Jack .. King] = [10]
    | otherwise             = [1 + fromEnum v]
  score' _                  = error "can not get score"

------------------------------------------------------------------------
directProcutList :: [[a]] -> [[a]]
directProcutList = foldl (\ls vs -> [ v : l | v <- vs, l <- ls ]) [[]]

------------------------------------------------------------------------
newtype CardS = CardS
  { cards :: [Card]
  } deriving (Show, Read, Eq, Ord)

------------------------------------------------------------------------
fullSetCards :: CardS
fullSetCards = CardS [ Card s v  Back |
                      s <- [Club .. Spade], v <- [Ace .. King] ]

------------------------------------------------------------------------
instance Viewable CardS where
  view (CardS cs) = CardS $ map view cs
  
instance Scoreable' CardS where
  score' (CardS cs) =
    let (s, b) = span (<22) $ sort $ map sum $
                 directProcutList $ map score' cs
    in if null s
       then [head b] -- == minimum b -- assume that b is sorted.
       else [last s] -- == maximum s -- assume that s is sorted.

instance Scoreable CardS -- where
  -- score cs = head $ score' cs

------------------------------------------------------------------------
------------------------------------------------------------------------
data BJPlayer = BJPlayer
  { name :: String
  , hand :: CardS
  } deriving
  ( Show
  )

------------------------------------------------------------------------
instance Viewable BJPlayer where
  view (BJPlayer n h) = BJPlayer n (view h)

instance Scoreable' BJPlayer where
  score' (BJPlayer _ h) = score' h

instance Scoreable BJPlayer

------------------------------------------------------------------------
addHand :: Card -> BJPlayer -> BJPlayer
addHand c (BJPlayer n (CardS h)) = BJPlayer n $ CardS (c:h)

------------------------------------------------------------------------
------------------------------------------------------------------------
updateList :: (Int, a) -> [a] -> [a]
updateList (i, v') l = left ++ [v'] ++ (tail right)
  where
    (left,right) = splitAt i l

------------------------------------------------------------------------
------------------------------------------------------------------------
data BJState = BJState
  { gen     :: StdGen
  , deck    :: CardS
  , players :: [BJPlayer]
  } deriving ( Show )

findPlayerBy :: (BJPlayer -> Bool) -> BJState -> Maybe (Int, BJPlayer)
findPlayerBy f ps = do
  pl  <- return $ players ps
  ind <- findIndex f pl
  return (ind, pl!!ind)

------------------------------------------------------------------------
findPlayer' :: String -> BJState -> Maybe (Int, BJPlayer)
findPlayer' n st = findPlayerBy ((n ==) . name) st

findPlayer :: String -> BJState -> BJPlayer
findPlayer n st = p
  where
    (_, p) = fromJust $ findPlayer' n st

------------------------------------------------------------------------
newtype Game a = Game
  { runGame :: ExceptT GamePhase (StateT BJState IO) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState BJState
  , MonadIO
  , MonadError GamePhase
  )

------------------------------------------------------------------------
initGame' :: Int -> BJState
initGame' seed = BJState
  (mkStdGen seed) fullSetCards
  [ (BJPlayer "Dealer" (CardS []))
  , (BJPlayer "Player" (CardS [])) ]

initGame :: Int -> Game ()
initGame = put . initGame'

------------------------------------------------------------------------
shuffleDeck' :: BJState -> BJState
shuffleDeck' (BJState g (CardS d) p) = BJState g' (CardS d') p
  where
    l = length d
    (ind, g') = runState (replicateM l $ state $ randomR (1,l)) g
    d' = map fst $ sortOn snd $ zip d ind

shuffleDeck :: Game ()
shuffleDeck = modify shuffleDeck'

------------------------------------------------------------------------
draw' :: (Card -> Card) -> (Card -> Card) -> String -> BJState -> (Card, BJState)
draw' op view pn s@(BJState g (CardS d) ps) =
  (c'', BJState g (CardS d') ps')
  where
    ([c], d') = splitAt 1 d
    c' = op c
    (i, p) = fromJust $ findPlayer' pn s
    ps' = updateList (i,addHand c' p) ps
    c'' = view c'

drawFront' :: String -> BJState -> (Card, BJState)
drawFront' = draw' frontCard view

drawFront :: String -> Game Card
drawFront = state . drawFront'

drawSelf' :: String -> BJState -> (Card, BJState)
drawSelf' = draw' id id

drawSelf :: String -> Game Card
drawSelf = state . drawSelf'

drawView' :: String -> BJState -> (Card, BJState)
drawView' = draw' id view

drawView :: String -> Game Card
drawView = state . drawView'

------------------------------------------------------------------------
------------------------------------------------------------------------
checkStat :: BJPlayer -> Game ()
checkStat p@(BJPlayer n _) = do
  score <- return $ score p
  if 22 > score
    then do liftIO $ putStrLn $ n ++ " Score: " ++ (show score)
    else do liftIO $ putStrLn $ n ++ " Score: " ++ (show score) ++ " Burst!"
            throwError GameOver

------------------------------------------------------------------------
type Action = BJPlayer -> Game Bool

------------------------------------------------------------------------
dealerAction :: Action
dealerAction p@(BJPlayer n _) = do
  score <- return $ score p
  if 17 > score
    then do c <- drawFront n
            liftIO $ putStrLn $ n ++ " draw " ++ (show c)
            return True
    else do return False

------------------------------------------------------------------------
playerAction :: Action
playerAction p@(BJPlayer n _) = do
  liftIO $ putStrLn $ n ++ ": Hit or Stand?[H/S]"
  cmd <- return . toUpper . head =<< liftIO getLine
  case cmd of
    'S' -> return False
    'H' -> do c <- drawSelf n
              liftIO $ putStrLn $ n ++ " draw " ++ (show c)
              return True
    _   -> do liftIO $ putStrLn "Bad Command : Hit or Stand?[H/S]"
              return False
  
------------------------------------------------------------------------
turn :: (BJPlayer -> Game Bool) -> String -> Game ()
turn action n = do
  gs <- get
  p <- return $ findPlayer n gs
  liftIO $ putStrLn $ show p
  checkStat p
  cont <- action p
  if cont
    then turn action n
    else return ()

------------------------------------------------------------------------
resoultGame' :: BJState -> (String, BJState)
resoultGame' gs
  | 21 >= ds && ds >= ps  = ("Win Dealer", gs)
  | 21 >= ps && ps >  ds  = ("Win Player", gs)
  | ps > 21               = ("Win Dealer", gs)
  | ds > 21               = ("Win Player", gs)
  | otherwise             = ("Draw", gs)
  where
    ds = score $ findPlayer "Dealer" gs
    ps = score $ findPlayer "Player" gs

resultGame :: Game String
resultGame = state resoultGame'

------------------------------------------------------------------------
gameExec' :: Game String
gameExec' = do
  shuffleDeck
  let n = "Player"
    in do c1 <- drawFront n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c1)
          c2 <- drawSelf  n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c2)
  let n = "Dealer"
    in do c1 <- drawFront n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c1)
          c2 <- drawView  n
          liftIO $ putStrLn $ n ++ " draw " ++ (show c2)
  ----------------------------------------------------------------------
  turn playerAction "Player"
  turn dealerAction "Dealer"
  ----------------------------------------------------------------------
  resultGame

gameExec :: Game String
gameExec = do
  catchError gameExec' (const resultGame)

------------------------------------------------------------------------
game :: Int -> IO ()
game seed = do
  run <- return $ runStateT $ runExceptT $ runGame gameExec
  (endState, _) <- run $ initGame' seed
  putStrLn $ show $ endState
