
import qualified Data.Set as S
import qualified System.Random as R
import qualified Data.Maybe as M

data Suit = Heart | Spade | Club | Diamond deriving (Show, Ord, Eq)
type Card = (Integer, Suit)
type Deck = S.Set Card
type Hand = [Card]
data GameState = GameState { myHand :: Hand
                           , houseHand :: Hand
                           , deck :: Deck
                           , myKeepDrawing :: Bool
                           , houseKeepDrawing :: Bool
                           } deriving Show

drawFromDeck :: Deck -> IO (Card, Deck)
drawFromDeck deck = do
  i <- R.randomRIO (0, S.size deck - 1)
  let card = S.elemAt i deck
  let deck' = S.deleteAt i deck
  return (card, deck')

myDrawFromDeck :: GameState -> IO GameState
myDrawFromDeck state = do
  (c, d) <- drawFromDeck $ deck state
  let h' = c `addToHand` (myHand state)
  return (GameState h' (houseHand state) d (myKeepDrawing state) (houseKeepDrawing state))

dealerDrawFromDeck :: GameState -> IO GameState
dealerDrawFromDeck state = do
  toDrawOrNot <- R.randomRIO (0,1)
  if (toDrawOrNot :: Integer) == 1
    then do
      (c, d) <- drawFromDeck $ deck state
      let h' = c `addToHand` (houseHand state)
      return (GameState (myHand state) h' d (myKeepDrawing state) (houseKeepDrawing state))   
    else
      return state

addToHand :: Card -> Hand -> Hand
addToHand = (:)

isBlackJack :: Card -> Bool
isBlackJack (n, suit) = n == 11 && elem suit [Spade, Club]

hasWon :: Hand -> Bool
hasWon h = or $ map (\x -> M.fromMaybe False $ x $ h) [(fmap isBlackJack) . maybeHead, Just . (==) 21 . sum . map fst]
  where
    maybeHead [] = Nothing
    maybeHead x = (Just . head) x

hasLost :: Hand -> Bool
hasLost h = (sum . map fst) h > 21

gameFinished :: GameState -> Bool
gameFinished (GameState mh hh _ _ _) = or . concat $ map (\x -> map x [mh, hh]) [hasWon, hasLost]

genInitialState :: GameState
genInitialState = GameState [] [] genInitialDeck True True
  where
    genInitialDeck = S.fromList cardsList
    cardsList = concat $ map (\c -> map (\n -> (n, c)) [2..14]) [Heart, Spade, Club, Diamond]

main :: IO ()
main = do
  putStrLn "Starting BlackJack!"
  let initialState = genInitialState
  programLoop initialState

programLoop :: GameState -> IO ()
programLoop state = do
  putStrLn $ "\nCurrent state: " ++ (show state)
  -- this is where we do our game calculations
  putStrLn $ "\nHey you! Your current hand is " ++ (show $ myHand state)
  putStrLn $ "Dealer has " ++ (show $ houseHand state)
  state' <- if myKeepDrawing state
              then do
                putStrLn "(s)top or (d)raw?"  
                c <- getChar
                if c == 'd'
                  then
                    myDrawFromDeck state
                  else
                    return (GameState (myHand state) (houseHand state) (deck state) False (houseKeepDrawing state))
              else
                return state
  state'' <- dealerDrawFromDeck state'
  -- decide whether to continue or not
  if gameFinished state''
    then do
      putStrLn "We have a winner!"
      putStrLn "Game Finished!"
    else programLoop state''
