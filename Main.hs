
import qualified Data.Set as S
import qualified System.Random as R

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

-- HELPER FUNCTION
-- safe version of head that accounts for NULL (Nothing)
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- DISPLAY FUNCTION
showHand :: Hand -> String
showHand h = (show h) ++ "\t(" ++ (show $ handSum h) ++ ")"

-- DISPLAY FUNCTION
-- assumes that the game has finished
showWinner :: GameState -> String
showWinner (GameState mh hh _ _ _)
  | isBlackJack hh                         = "House"
  | isBlackJack mh                         = "You!"
  | handSum mh > 21                        = "House"
  | handSum hh > 21                        = "You!"
  | (21 - handSum hh) <= (21 - handSum mh) = "House"
  | otherwise                              = "You!"

-- SETUP FUNCTION
genInitialState :: GameState
genInitialState = GameState [] [] genInitialDeck True True
  where
    genInitialDeck = S.fromList cardsList
    cardsList = concat $ map (\c -> map (\n -> (n, c)) [2..14]) [Heart, Spade, Club, Diamond]

-- GAME LOGIC FUNCTIONS

-- whether the last card drawn was a blackjack (accounts for NULL/Nothing)
isBlackJack :: Hand -> Bool
isBlackJack = (maybe False isBlackJack') . maybeHead
  where
    isBlackJack' (n, suit) = n == 11 && elem suit [Spade, Club]

handSum :: Hand -> Integer
handSum = sum . map fst

addToHand :: Card -> Hand -> Hand
addToHand c h = (convertAce c) : h
  where
    -- automatic conversion of aces if hand goes over
    convertAce (num, suit) =
      if num == 14 && handSum h > 7
        then (1, suit)
        else (num, suit)

hasWon :: Hand -> Bool
hasWon = \h -> or $ map (\x -> x $ h) [isBlackJack, (==) 21 . handSum]

hasLost :: Hand -> Bool
hasLost = ((> 21) . handSum)

gameFinished :: GameState -> Bool
gameFinished (GameState mh hh _ mk hk) = or $ ((:) $ not (mk || hk)) $ concat $ map (\x -> map x [mh, hh]) [hasWon, hasLost]

-- IO FUNCTIONS (USER INPUT/RANDOM GEN/GAME LOOP)

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
dealerDrawFromDeck state =
  let
    -- define our AI Behavior using a function that will have higher chances
    --  of drawing more cards if the hand is far below 21
    aiFunction h = -(round magicFun) - 1
      where
        hSum = handSum h
        -- numerical computation delta == 0.01 to avoid asymptote at 21
        magicFun = 441 / ((fromIntegral hSum - 21.01) * (fromIntegral hSum + 21))
  in do
    -- we want the AI to be less likely to draw if their hand is too large
    toDrawOrNot <- R.randomRIO (0, aiFunction $ houseHand state) :: IO Integer
    if toDrawOrNot == 0 && houseKeepDrawing state
      then do
        (c, d) <- drawFromDeck $ deck state
        let h' = c `addToHand` (houseHand state)
        return (GameState (myHand state) h' d (myKeepDrawing state) (houseKeepDrawing state))   
      else
        return (GameState (myHand state) (houseHand state) (deck state) (myKeepDrawing state) False)

programLoop :: GameState -> IO ()
programLoop state = do
  putStrLn $ "\nCurrent state: " ++ (show state)
  -- this is where we do our game calculations
  putStrLn $ "\nHey you! Your current hand is " ++ (showHand $ myHand state)
  putStrLn $ "The house's current hand is " ++ (showHand $ houseHand state)
  state' <- if myKeepDrawing state
              then do
                putStrLn "(s)top or (d)raw?"  
                c <- getLine
                if c == "d"
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
      putStrLn "\nWe have a winner!"
      putStrLn $ show state''
      putStrLn "Game Finished!"
      putStrLn $ "\nYour current hand is " ++ (showHand $ myHand state'')
      putStrLn $ "The house's current hand is " ++ (showHand $ houseHand state'')
      putStrLn $ "\nThe winner is: " ++ (showWinner state'')
    else programLoop state''

main :: IO ()
main = do
  putStrLn "Starting BlackJack!"
  let initialState = genInitialState
  programLoop initialState
