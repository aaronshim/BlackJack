
import Data.Set

data Suit = Heart | Spade | Club | Diamond deriving Show
type Card = (Int, Suit)
type Deck = Set Card
type Hand = [Card]
data GameState = GameState (Hand) (Hand) (Deck) deriving Show

main :: IO ()
main = do
  putStrLn "Hello, world!"
