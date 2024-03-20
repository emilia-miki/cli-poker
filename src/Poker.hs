module Poker where

import qualified System.Random as R
import qualified System.Random.Shuffle as RS
import Data.List.Split (chunksOf)
import Data.List (elemIndex, maximumBy)

data Suit where
  Clubs :: Suit
  Spades :: Suit
  Hearts :: Suit
  Diamonds :: Suit
  deriving (Eq, Show)

data Rank where
  Two :: Rank
  Three :: Rank
  Four :: Rank
  Five :: Rank
  Six :: Rank
  Seven :: Rank
  Eight :: Rank
  Nine :: Rank
  Ten :: Rank
  Jack :: Rank
  Queen :: Rank
  King :: Rank
  Ace :: Rank
  deriving (Eq, Ord, Show)

data Card = Card Suit Rank deriving Show

instance Eq Card where
  (==) (Card _ r1) (Card _ r2) = r1 == r2

instance Ord Card where
  (<=) (Card _ r1) (Card _ r2) = r1 <= r2

data Combination where
  RoyalFlush :: Combination
  StraightFlush :: Card -> Card -> Card -> Combination
  FourOfAKind :: Card -> Card -> Card -> Combination
  FullHouse :: Card -> Card -> Card -> Card -> Combination
  Flush :: Card -> Card -> Card -> Combination
  Straight :: Card -> Card -> Card -> Combination
  ThreeOfAKind :: Card -> Card -> Card -> Combination
  TwoPair :: Card -> Card -> Card -> Card -> Combination
  Pair :: Card -> Card -> Card -> Combination
  HighCard :: Card -> Card -> Card -> Combination
  deriving (Eq, Ord, Show)

data Chip = C5 | C10 | C25 | C50 deriving (Eq, Ord, Show)

data Role = Dealer | SmallBlind | BigBlind | None deriving Show

data Player = Player {
  chips :: [Chip],
  cards :: [Card],
  role :: Role
}

data Poker = Poker {
  deck :: [Card],
  players :: [Player],
  playerIdx :: Int,
  bet :: [Chip],
  bank :: [Chip],
  table :: [Card]
}

dealCards :: Poker -> Poker
dealCards p = let (dealt, deck') = deal (2 * length (players p)) (deck p)
              in p {deck = deck', players = dealToPlayers dealt (players p)}
  where dealToPlayers cards players = zipWith (\ p' c -> p' {cards = c}) players (chunksOf 2 cards)

deal :: Int -> [Card] -> ([Card], [Card])
deal = splitAt

nextRound :: Poker -> IO (Maybe Poker)
nextRound p = do
  deck <- shuffle
  let winner = determineWinner p
  let newP = startRound p {
    deck = deck,
    bet = [],
    bank = [],
    table = [],
    playerIdx = 0,
    players = rotatePlayers $ giveBank (bank p) winner (players p)
  }
  if isPlayable newP then return (Just newP) else return Nothing

isPlayable :: Poker -> Bool
isPlayable p = length (players p) > 1 && has C5 (head $ players p) && has C10 (head $ tail $ players p)
  where has C5 player = not $ null $ chips player
        has C10 player = elem C10 (chips player) || length (filter (== C5) (chips player)) > 1
        has _ _ = undefined

determineWinner :: Poker -> Int
determineWinner p = fst $ maximumBy (\x y -> compare (snd x) (snd y)) idxCombos
  where combinations = map (determineCombination (table p)) (players p)
        idxCombos = zip [0..] combinations

determineCombination :: [Card] -> Player -> Combination
determineCombination = undefined

rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = undefined
rotatePlayers (p:rest) = mappend rest [p]

giveBank :: [Chip] -> Int -> [Player] -> [Player]
giveBank bank winner players = concat [take winner players, [winnerPlayer'], drop (winner + 1) players]
  where winnerPlayer = players !! winner
        winnerPlayer' = winnerPlayer {chips = mappend (chips winnerPlayer) bank}

initPoker :: Int -> IO Poker
initPoker n = do
  deck <- shuffle
  return Poker {
    deck = deck,
    players = initPlayers n,
    bet = [],
    bank = [],
    table = [],
    playerIdx = 0
  }

startRound :: Poker -> Poker
startRound = takeBigBlind . takeSmallBlind . dealCards

takeSmallBlind :: Poker -> Poker
takeSmallBlind p = case maybeChipIdx of
    Just idx -> let blind' = blind {
                    chips = take idx (chips blind) ++ drop (idx + 1) (chips blind)
                  }
                in p {
                    players = blind' : tail (players p),
                    bank = C5 : bank p,
                    playerIdx = playerIdx p + 1
                  }
    Nothing -> takeSmallBlind $ p {players = tail (players p)}
  where blind = head (players p)
        maybeChipIdx = findChip C5 (chips blind)

takeBigBlind :: Poker -> Poker
takeBigBlind p = case maybeChipIdx of
    Just idx -> let blind' = blind {
                    chips = take idx (chips blind) ++ drop (idx + 1) (chips blind)
                  }
                in p {
                    players = head (players p) : blind' : drop 2 (players p),
                    bank = C10 : bank p,
                    playerIdx = playerIdx p + 1
                  }
    Nothing -> takeBigBlind $ p {players = head (players p) : drop 2 (players p)}
  where blind = head $ tail $ players p
        maybeChipIdx = findChip C10 (chips blind)

findChip :: Chip -> [Chip] -> Maybe Int
findChip chip chips = case elemIndex chip chips of
  Just idx -> Just idx
  Nothing -> findChip chip $ exchangeChipFor chip chips

exchangeChipFor :: Chip -> [Chip] -> [Chip]
exchangeChipFor chip chips = go chip 0
  where go :: Chip -> Int -> [Chip]
        go ch idx
          | idx >= length chips = chips
          | (chips !! idx) > ch = exchangeChipFor' ch (chips !! idx)
          | otherwise = go ch (idx + 1)
        exchangeChipFor' :: Chip -> Chip -> [Chip]
        exchangeChipFor' C5 C10 = replicate 2 C5
        exchangeChipFor' C5 C25 = replicate 5 C5
        exchangeChipFor' C5 C50 = replicate 10 C5
        exchangeChipFor' C10 C25 = [C10, C10, C5]
        exchangeChipFor' C10 C50 = replicate 5 C10
        exchangeChipFor' C25 C50 = replicate 2 C25
        exchangeChipFor' _ _ = undefined

initPlayers :: Int -> [Player]
initPlayers n = replicate n initPlayer

initPlayer :: Player
initPlayer = Player {
  chips = initChips,
  cards = [],
  role = None
}

initChips :: [Chip]
initChips = concat [replicate 4 C50, replicate 6 C25, replicate 10 C10, replicate 10 C5]

initDeck :: [Card]
initDeck = map (uncurry Card) $ liftA2 (,) suits ranks
  where suits = [Clubs, Spades, Hearts, Diamonds]
        ranks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

initDeckSize :: Int
initDeckSize = 52

shuffle :: IO [Card]
shuffle = do
  RS.shuffle' initDeck initDeckSize <$> R.initStdGen
