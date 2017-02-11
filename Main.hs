
import Data.Set

data Suit = Heart | Spade | Club | Diamond
type Card = (Int, Suit)
type Deck = Set Card
type Hand = [Card]
data GameState = GameState (Hand) (Hand) (Deck)

main :: IO ()
main = do
  putStrLn "Hello, world!"
