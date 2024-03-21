module Poker where

import Data.List (maximumBy, sortBy)
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Ord (Down (Down), comparing)
import System.Random qualified as R
import System.Random.Shuffle qualified as RS

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

data Card = Card Suit Rank deriving (Show)

instance Eq Card where
  (==) (Card _ r1) (Card _ r2) = r1 == r2

instance Ord Card where
  (<=) (Card _ r1) (Card _ r2) = r1 <= r2

data Combination where
  -- Cards from highest to lowest
  HighCard :: Card -> Card -> Card -> Card -> Card -> Combination
  -- First the pair, then the rest of cards from highest to lowest
  Pair :: Card -> Card -> Card -> Card -> Card -> Combination
  -- First the higher pair, then the lower pair and then the other card
  TwoPair :: Card -> Card -> Card -> Card -> Card -> Combination
  -- First the three, then the two other cards from highest to lowest
  ThreeOfAKind :: Card -> Card -> Card -> Card -> Card -> Combination
  -- The cards of the straight from highest to lowest
  Straight :: Card -> Card -> Card -> Card -> Card -> Combination
  -- All cards in the flush from highest to lowest
  Flush :: Card -> Card -> Card -> Card -> Card -> Combination
  -- First the three, then the two
  FullHouse :: Card -> Card -> Card -> Card -> Card -> Combination
  -- First the four, then the other card
  FourOfAKind :: Card -> Card -> Card -> Card -> Card -> Combination
  -- The highest card of the straight flush
  StraightFlush :: Card -> Combination
  -- No arguments needed
  RoyalFlush :: Suit -> Combination
  deriving (Eq, Show)

instance Ord Combination where
  compare (RoyalFlush _) (RoyalFlush _) = EQ
  compare (RoyalFlush _) _ = GT
  compare (StraightFlush c1) (StraightFlush c2) = compare c1 c2
  compare (StraightFlush _) _ = GT
  compare (FourOfAKind c1 _ _ _ c5) (FourOfAKind c1' _ _ _ c5') =
    case compare c1 c1' of
      EQ -> compare c5 c5'
      neq -> neq
  compare (FourOfAKind {}) _ = GT
  compare (FullHouse c1 _ _ c4 _) (FullHouse c1' _ _ c4' _) =
    uCompare [c1, c4] [c1', c4']
  compare (FullHouse {}) _ = GT
  compare (Flush c1 c2 c3 c4 c5) (Flush c1' c2' c3' c4' c5') =
    uCompare [c1, c2, c3, c4, c5] [c1', c2', c3', c4', c5']
  compare (Flush {}) _ = GT
  compare (Straight c1 _ _ _ _) (Straight c1' _ _ _ _) =
    compare c1 c1'
  compare (Straight {}) _ = GT
  compare (ThreeOfAKind c1 _ _ c4 c5) (ThreeOfAKind c1' _ _ c4' c5') =
    case compare c1 c1' of
      EQ -> case compare c4 c4' of
        EQ -> compare c5 c5'
        neq -> neq
      neq -> neq
  compare (ThreeOfAKind {}) _ = GT
  compare (TwoPair c1 _ c3 _ c5) (TwoPair c1' _ c3' _ c5') =
    case uCompare [c1, c3] [c1', c3'] of
      EQ -> compare c5 c5'
      neq -> neq
  compare (TwoPair {}) _ = GT
  compare (Pair c1 c2 c3 c4 c5) (Pair c1' c2' c3' c4' c5') =
    case compare c1 c1' of
      EQ -> uCompare [c2, c3, c4, c5] [c2', c3', c4', c5']
      neq -> neq
  compare (Pair {}) _ = GT
  compare (HighCard c1 c2 c3 c4 c5) (HighCard c1' c2' c3' c4' c5') =
    uCompare [c1, c2, c3, c4, c5] [c1', c2', c3', c4', c5']
  compare (HighCard {}) _ = LT

uCompare :: (Ord a) => [a] -> [a] -> Ordering
uCompare l1 l2 = compare (sortBy (comparing Down) l1) (sortBy (comparing Down) l2)

data Chip = C5 | C10 | C25 | C50 deriving (Eq, Ord, Show)

data Role = Dealer | SmallBlind | BigBlind | None deriving (Show)

data Play = Fold | Check | Open Int | Call Int | Raise Int | AllIn Int deriving (Show)

data Status = Folded | Bet Int | AllIned Int deriving (Eq, Show)

data Player = Player
  { chips :: Int,
    cards :: [Card],
    status :: Status
  }
  deriving (Show)

data Poker = Poker
  { deck :: [Card],
    players :: [Player],
    playerIdx :: Int,
    bank :: M.Map Int Int,
    currentBet :: Int,
    table :: [Card]
  }
  deriving (Show)

dealCards :: Poker -> Poker
dealCards p =
  let (dealt, deck') = deal (2 * length (players p)) (deck p)
   in p {deck = deck', players = dealToPlayers dealt (players p)}
  where
    dealToPlayers cards players = zipWith (\p' c -> p' {cards = c}) players (chunksOf 2 cards)

deal :: Int -> [Card] -> ([Card], [Card])
deal = splitAt

nextGame :: Poker -> IO (Maybe Poker)
nextGame p = do
  deck <- shuffle
  let winner = determineWinner p
  let newP =
        startRound
          p
            { deck = deck,
              bank = M.empty,
              currentBet = 0,
              table = [],
              playerIdx = 0,
              players = rotatePlayers $ giveBank (bank p) winner (players p)
            }
  if isPlayable newP then return (Just newP) else return Nothing

nextTurn :: Poker -> Poker
nextTurn p = case table p of
  [] ->
    let (dealt, deck') = deal 3 (deck p)
     in p {deck = deck', table = dealt}
  t
    | length t == 3 ->
        let (dealt, deck') = deal 1 (deck p)
         in p {deck = deck', table = t ++ dealt}
  t
    | length t == 4 ->
        let (dealt, deck') = deal 1 (deck p)
         in p {deck = deck', table = t ++ dealt}
  _other -> undefined

isPlayable :: Poker -> Bool
isPlayable p =
  length (players p) > 1 && chips (head $ players p) >= 5 && chips (head $ tail $ players p) >= 10

determineWinner :: Poker -> Int
determineWinner p = fst $ maximumBy (\x y -> compare (snd x) (snd y)) idxCombos
  where
    combinations = map (determineCombination (table p)) (players p)
    idxCombos = zip [0 ..] combinations

determineCombination :: [Card] -> Player -> Combination
determineCombination table player = go getters getHighCard Nothing (sortBy (comparing Down) $ table ++ cards player)
  where
    go :: [[Card] -> Maybe Combination] -> ([Card] -> Combination) -> Maybe Combination -> [Card] -> Combination
    go [] lf Nothing c = lf c
    go [] _ (Just combo) _ = combo
    go (f : fs) lf pc c = case pc of
      Just combo -> combo
      Nothing -> go fs lf (f c) c
    getters :: [[Card] -> Maybe Combination]
    getters =
      [ getRoyalFlush,
        getStraightFlush,
        getFourOfAKind,
        getFullHouse,
        getFlush,
        getStraight,
        getThreeOfAKind,
        getTwoPair,
        getPair
      ]

getRoyalFlush :: [Card] -> Maybe Combination
getRoyalFlush (Card s1 Ace : Card s2 King : Card s3 Queen : Card s4 Jack : Card s5 Ten : _) =
  if all (== s1) [s2, s3, s4, s5]
    then Just $ RoyalFlush s1
    else Nothing
getRoyalFlush _ = Nothing

getStraightFlush :: [Card] -> Maybe Combination
getStraightFlush [] = Nothing
getStraightFlush (highest : rest)
  | length rest < 4 = Nothing
  | otherwise =
      let cards = highest : take 4 rest
       in let zipped = zip cards (tail cards)
           in if all go zipped
                then Just $ StraightFlush highest
                else getStraightFlush rest
  where
    go (Card s1 r1, Card s2 r2) = s1 == s2 && isDescByOne r1 r2

isDescByOne :: Rank -> Rank -> Bool
isDescByOne Ace King = True
isDescByOne King Queen = True
isDescByOne Queen Jack = True
isDescByOne Jack Ten = True
isDescByOne Ten Nine = True
isDescByOne Nine Eight = True
isDescByOne Eight Seven = True
isDescByOne Seven Six = True
isDescByOne Six Five = True
isDescByOne Five Four = True
isDescByOne Four Three = True
isDescByOne Three Two = True
isDescByOne Two Ace = True
isDescByOne _ _ = False

getFourOfAKind :: [Card] -> Maybe Combination
getFourOfAKind cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go :: Int -> Maybe Combination
    go idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 4 = construct idx
      | idx == 3 = Nothing
      | otherwise = go (idx + 1)
    construct 0 = Just $ FourOfAKind (head four) (four !! 1) (four !! 2) (four !! 3) other
      where
        four = filter (\c -> c == head cards) cards
        other = head (filter (\c -> c /= head cards) cards)
    construct idx = Just $ FourOfAKind other (head four) (four !! 1) (four !! 2) (four !! 3)
      where
        four = filter (\c -> c == (cards !! idx)) cards
        other = head (filter (\c -> c /= (cards !! idx)) cards)

getFullHouse :: [Card] -> Maybe Combination
getFullHouse cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go :: Int -> Maybe Combination
    go idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 3 = go' idx 0
      | idx == 4 = Nothing
      | otherwise = go (idx + 1)
    go' :: Int -> Int -> Maybe Combination
    go' idx' idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 2 = construct idx' idx
      | idx == 5 = Nothing
      | otherwise = go' idx' (idx + 1)
    construct idx3 idx2 =
      let three = filter (\c -> c == (cards !! idx3)) cards
       in let two = filter (\c -> c == (cards !! idx2)) cards
           in Just $ FullHouse (head three) (three !! 1) (three !! 2) (head two) (two !! 1)

getFlush :: [Card] -> Maybe Combination
getFlush cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go idx
      | length (filter (\c -> suit c == suit (cards !! idx)) cards) == 5 = construct idx
      | idx == 2 = Nothing
      | otherwise = go (idx + 1)
    construct idx = Just $ Flush (head cds) (cds !! 1) (cds !! 2) (cds !! 3) (cds !! 4)
      where
        cds = filter (\c -> suit c == suit (cards !! idx)) cards

suit :: Card -> Suit
suit (Card s _) = s

getStraight :: [Card] -> Maybe Combination
getStraight [] = Nothing
getStraight (highest : rest)
  | length rest < 4 = Nothing
  | otherwise =
      let cards = highest : take 4 rest
       in let ranks = map (\(Card _ r) -> r) cards
           in let zipped = zip ranks (tail ranks)
               in if all (uncurry isDescByOne) zipped
                    then construct cards
                    else getStraight rest
  where
    construct cards = Just $ Straight (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)

getThreeOfAKind :: [Card] -> Maybe Combination
getThreeOfAKind cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 3 = construct idx
      | idx == 4 = Nothing
      | otherwise = go (idx + 1)
    construct idx = Just $ ThreeOfAKind (head three) (three !! 1) (three !! 2) (head other) (other !! 1)
      where
        three = filter (\c -> c == (cards !! idx)) cards
        other = filter (\c -> c /= (cards !! idx)) cards

getTwoPair :: [Card] -> Maybe Combination
getTwoPair cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 2 = go' idx (idx + 1)
      | idx == 5 = Nothing
      | otherwise = go (idx + 1)
    go' idx' idx
      | length (filter (\c -> c == (cards !! idx) && c /= (cards !! idx')) cards) == 2 =
          construct idx' idx
      | idx == 5 = Nothing
      | otherwise = go' idx' (idx + 1)
    construct idx idx' = Just $ TwoPair (head pair1) (pair1 !! 1) (head pair2) (pair2 !! 1) other
      where
        (higherPair, lowerPair) =
          if (cards !! idx) > (cards !! idx')
            then (idx, idx')
            else (idx', idx)
        pair1 = filter (\c -> c == (cards !! higherPair)) cards
        pair2 = filter (\c -> c == (cards !! lowerPair)) cards
        other = head $ filter (\c -> c /= (cards !! higherPair) && c /= (cards !! lowerPair)) cards

getPair :: [Card] -> Maybe Combination
getPair cards
  | length cards /= 7 = Nothing
  | otherwise = go 0
  where
    go idx
      | length (filter (\c -> c == (cards !! idx)) cards) == 2 = construct idx
      | idx == 5 = Nothing
      | otherwise = go (idx + 1)
    construct idx = Just $ Pair (head pair) (pair !! 1) (head other) (other !! 1) (other !! 2)
      where
        pair = filter (\c -> c == (cards !! idx)) cards
        other = filter (\c -> c /= (cards !! idx)) cards

getHighCard :: [Card] -> Combination
getHighCard cards = HighCard (head cards) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)

rotatePlayers :: [Player] -> [Player]
rotatePlayers [] = undefined
rotatePlayers (p : rest) = mappend rest [p]

giveBank :: M.Map Int Int -> Int -> [Player] -> [Player]
giveBank bank winner players = concat [take winner players, [winnerPlayer'], drop (winner + 1) players]
  where
    winnerPlayer = players !! winner
    winnerPlayer' = winnerPlayer {chips = chips winnerPlayer + sum (map snd (M.toList bank))}

initPoker :: Int -> IO (Maybe Poker)
initPoker n = do
  if n > 1
    then do
      deck <- shuffle
      return $
        Just
          Poker
            { deck = deck,
              players = initPlayers n,
              currentBet = 0,
              bank = M.empty,
              table = [],
              playerIdx = 0
            }
    else return Nothing

startRound :: Poker -> Poker
startRound = takeBigBlind . takeSmallBlind . dealCards

takeSmallBlind :: Poker -> Poker
takeSmallBlind p =
  if chips blind >= 5
    then
      p
        { players = blind' : tail (players p),
          bank = M.adjust (+ 5) 0 (bank p),
          playerIdx = playerIdx p + 1
        }
    else takeSmallBlind $ p {players = tail (players p)}
  where
    blind = head (players p)
    blind' = blind {chips = chips blind - 5}

takeBigBlind :: Poker -> Poker
takeBigBlind p =
  if chips blind >= 10
    then
      p
        { players = head (players p) : blind' : drop 2 (players p),
          bank = M.adjust (+ 10) 1 (bank p),
          playerIdx = playerIdx p + 1
        }
    else takeBigBlind $ p {players = head (players p) : drop 2 (players p)}
  where
    blind = head $ tail $ players p
    blind' = blind {chips = chips blind - 10}

exchangeChipFor :: Chip -> [Chip] -> [Chip]
exchangeChipFor chip chips = go chip 0
  where
    go :: Chip -> Int -> [Chip]
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
initPlayer =
  Player
    { chips = 500,
      cards = [],
      status = Bet 0
    }

getRole :: Int -> Poker -> Role
getRole 0 _ = SmallBlind
getRole 1 _ = BigBlind
getRole idx p =
  if (length (players p) > 2) && (idx == length (players p) - 1) then Dealer else None

getChips :: Poker -> Int
getChips p = chips (players p !! playerIdx p)

initChips :: [Chip]
initChips = concat [replicate 4 C50, replicate 6 C25, replicate 10 C10, replicate 10 C5]

initDeck :: [Card]
initDeck = map (uncurry Card) $ liftA2 (,) suits ranks
  where
    suits = [Clubs, Spades, Hearts, Diamonds]
    ranks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

initDeckSize :: Int
initDeckSize = 52

shuffle :: IO [Card]
shuffle = do
  RS.shuffle' initDeck initDeckSize <$> R.initStdGen

play :: Play -> Poker -> Poker
play pl p = case pl of
  Fold -> skipFolded $ p {players = playersFold (playerIdx p) (players p)}
  Check -> skipFolded $ movePlayer p
  Open bet -> go bet p
  Call bet -> go bet p
  Raise bet -> go bet p
  AllIn bet -> go bet p
  where
    go bet' p' =
      skipFolded $
        movePlayer $
          p'
            { players = playersBet bet' (playerIdx p') (players p'),
              currentBet = bet' + currentBet p'
            }

toChips :: Int -> [Chip]
toChips num =
  concat
    [ replicate (num `div` 50) C50,
      replicate ((num `mod` 50) `div` 25) C25,
      replicate ((num `mod` 25) `div` 10) C10,
      replicate ((num `mod` 10) `div` 5) C5
    ]

playersBet :: Int -> Int -> [Player] -> [Player]
playersBet bet idx players = take idx players ++ [player'] ++ drop (idx + 1) players
  where
    player = players !! idx
    player' = player {status = Bet bet}

isWon :: Poker -> Bool
isWon p = length (filter (\pl -> status pl /= Folded) (players p)) == 1

isRoundEnd :: Poker -> Bool
isRoundEnd p =
  let bets = map getBet $ filter isBet $ map status (players p)
   in all (uncurry (==)) $ zip bets (tail bets)

isBet :: Status -> Bool
isBet (Bet _) = True
isBet _ = False

getBet :: Status -> Int
getBet (Bet bet) = bet
getBet _ = undefined

movePlayer :: Poker -> Poker
movePlayer p = p {playerIdx = (playerIdx p + 1) `mod` length (players p)}

skipFolded :: Poker -> Poker
skipFolded p = case status (players p !! playerIdx p) of
  Folded -> skipFolded $ movePlayer p
  _other -> p

playersFold :: Int -> [Player] -> [Player]
playersFold idx players = take idx players ++ [player'] ++ drop (idx + 1) players
  where
    player = players !! idx
    player' = player {status = Folded}

getPlays :: Poker -> [Play]
getPlays p = case status player of
  Folded -> []
  AllIned _ -> []
  Bet bet -> case currentBet p of
    0 -> [Check, AllIn (chips player), Open 0]
    shared ->
      if shared == bet
        then [Check, Raise 0, AllIn (chips player)]
        else
          if shared - bet >= chips player
            then [Fold, AllIn (chips player)]
            else [Fold, Call shared]
  where
    player = players p !! playerIdx p
