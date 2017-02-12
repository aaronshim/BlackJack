
import qualified Data.Set as S

data Suit = Heart | Spade | Club | Diamond deriving (Show, Ord, Eq)
type Card = (Integer, Suit)
type Deck = S.Set Card
type Hand = [Card]
data GameState = GameState { myHand :: Hand
                           , houseHand :: Hand
                           , deck :: Deck
                           } deriving Show

--drawFromDeck :: Deck -> IO (Card, Deck)

--addToHand :: Hand -> Card -> Hand

--hasWon :: Hand -> Bool

--hasLost :: Hand -> Bool

--gameFinished :: State -> Bool

genInitialState :: GameState
genInitialState = GameState [] [] genInitialDeck
  where
    genInitialDeck = S.fromList cardsList
    cardsList = concat $ map (\c -> map (\n -> (n, c)) [2..14]) [Heart, Spade, Club, Diamond]

main :: IO ()
main = do
  putStrLn "Hello, world!"
  let initialState = genInitialState
  programLoop initialState

programLoop :: GameState -> IO ()
programLoop state = do
  putStrLn $ "Current state: " ++ (show state)
  -- this is where we do our game calculations
  let state' = state
  -- then decide to continue or finish
  programLoop state
