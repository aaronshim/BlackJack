
import Data.Set

data Suit = Heart | Spade | Club | Diamond deriving Show
type Card = (Int, Suit)
type Deck = Set Card
type Hand = [Card]
data GameState = GameState (Hand) (Hand) (Deck) deriving Show

--drawFromDeck :: Deck -> IO (Card, Deck)

--addToHand :: Hand -> Card -> Hand

--hasWon :: Hand -> Bool

--hasLost :: Hand -> Bool

--gameFinished :: State -> Bool

main :: IO ()
main = do
  putStrLn "Hello, world!"
