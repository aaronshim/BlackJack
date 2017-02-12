
import qualified Data.Set as S

data Suit = Heart | Spade | Club | Diamond deriving (Show, Ord, Eq)
type Card = (Integer, Suit)
type Deck = S.Set Card
type Hand = [Card]
data GameState = GameState { myHand :: Hand
                           , houseHand :: Hand
                           , deck :: Deck
                           } deriving Show

--myDrawFromDeck :: GameState -> IO GameState
myDrawFromDeck = return

--dealerDrawFromDeck :: GameState -> IO GameState
dealerDrawFromDeck = return

addToHand :: Card -> Hand -> Hand
addToHand = (:)

isBlackJack :: Card -> Bool
isBlackJack (n, suit) = n == 11 && elem suit [Spade, Club]

hasWon :: Hand -> Bool
hasWon h = and $ map (\x -> x $ h) [isBlackJack . head, (==) 21 . sum . map fst]

hasLost :: Hand -> Bool
hasLost h = (sum . map fst) h > 21

gameFinished :: GameState -> Bool
gameFinished (GameState mh hh _) = and . concat $ map (\x -> map x [mh, hh]) [hasWon, hasLost]

genInitialState :: GameState
genInitialState = GameState [] [] genInitialDeck
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
  putStrLn "(s)top or (d)raw?"  
  c <- getChar
  state' <- if c == 'd' then myDrawFromDeck state else return state
  state'' <- dealerDrawFromDeck state'
  -- decide whether to continue or not
  if gameFinished state''
    then do
      putStrLn "We have a winner!"
      putStrLn "Game Finished!"
    else programLoop state''
