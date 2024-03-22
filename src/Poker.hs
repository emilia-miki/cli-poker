module Poker
  ( Suit (..),
    Rank (..),
    Card (..),
    Combination (..),
    Chip (..),
    Role (..),
    Play (..),
    Player (..),
    Poker (..),
    startGame,
    nextGame,
    nextRound,
    play,
    getCombinations,
    getWinner,
    getPlays,
    getRole,
    getChips,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (findIndex, maximumBy, sortBy)
import Data.List.Split (chunksOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import GHC.Generics (Generic)
import System.Random qualified as R
import System.Random.Shuffle qualified as RS

-- This section defines types

data Suit where
  Clubs :: Suit
  Spades :: Suit
  Hearts :: Suit
  Diamonds :: Suit
  deriving (Eq, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

data Card = Card Suit Rank deriving (Show, Generic)

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
  deriving (Eq, Show, Generic)

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

data Chip = C5 | C10 | C25 | C50 deriving (Eq, Ord, Show, Generic)

data Role = Dealer | SmallBlind | BigBlind | None deriving (Show, Generic)

data Play = Fold | Check | Open Int | Call Int | Raise Int | AllIn Int deriving (Eq, Show, Generic)

data Status = Init | Folded | Bet Int | AllIned Int deriving (Eq, Show, Generic)

instance ToJSON Chip

instance ToJSON Role

instance ToJSON Play

instance ToJSON Status

instance ToJSON Suit

instance ToJSON Rank

instance ToJSON Card

instance ToJSON Combination

instance ToJSON Player

instance FromJSON Play

data Player = Player
  { name :: String,
    chips :: Int,
    cards :: [Card],
    status :: Status
  }
  deriving (Show, Generic)

data Poker = Poker
  { deck :: [Card],
    players :: [Player],
    playerIdx :: Int,
    bank :: M.Map Int Int,
    currentBet :: Int,
    table :: [Card]
  }
  deriving (Show)

-- this section defines exported functions changing the game state

startGame :: [String] -> IO (Maybe Poker)
startGame usernames = do
  p <- initPoker usernames
  return $ initGame p
  where
    initPoker :: [String] -> IO Poker
    initPoker names = do
      deck <- shuffle
      return
        Poker
          { deck = deck,
            players = map initPlayer names,
            currentBet = 0,
            bank = M.empty,
            table = [],
            playerIdx = 0
          }
    initPlayer :: String -> Player
    initPlayer username =
      Player
        { name = username,
          chips = 500,
          cards = [],
          status = Init
        }

nextGame :: Poker -> IO (Maybe Poker)
nextGame poker = do
  deck <- shuffle
  let poker' = giveBank (fromJust $ getWinner poker) poker'
  return $
    initGame
      poker'
        { deck = deck,
          players = map resetStatus $ rotatePlayers $ players poker',
          currentBet = 0,
          bank = M.empty,
          table = [],
          playerIdx = 0
        }
  where
    resetStatus :: Player -> Player
    resetStatus player = player {status = Init}
    rotatePlayers :: [Player] -> [Player]
    rotatePlayers [] = undefined
    rotatePlayers (p' : rest) = rest ++ [p']
    giveBank :: String -> Poker -> Poker
    giveBank winnerName p = p {players = players''}
      where
        winnerIdx = fromJust $ findIndex (\pl -> name pl == winnerName) (players p)
        winner = players p !! winnerIdx
        winner' = winner {chips = chips winner + sum (map snd (M.toList (bank poker)))}
        players'' = concat [take winnerIdx (players poker), [winner'], drop (winnerIdx + 1) (players poker)]

nextRound :: Poker -> Maybe Poker
nextRound p = case table p of
  [] ->
    let (dealt, deck') = deal 3 (deck p)
     in postprocess $ p {deck = deck', table = dealt}
  t
    | length t == 3 ->
        let (dealt, deck') = deal 1 (deck p)
         in postprocess $ p {deck = deck', table = t ++ dealt}
  t
    | length t == 4 ->
        let (dealt, deck') = deal 1 (deck p)
         in postprocess $ p {deck = deck', table = t ++ dealt}
  _other -> Nothing
  where
    resetStatus p' = p' {players = players'}
      where
        resetStatus' Folded = Folded
        resetStatus' _ = Init
        players' = map (\pl -> pl {status = resetStatus' (status pl)}) (players p)
    postprocess p' = Just $ (resetStatus p') {currentBet = 0, playerIdx = 0}

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
    playersFold :: Int -> [Player] -> [Player]
    playersFold idx players = take idx players ++ [player'] ++ drop (idx + 1) players
      where
        player = players !! idx
        player' = player {status = Folded}
    playersBet :: Int -> Int -> [Player] -> [Player]
    playersBet bet idx players = take idx players ++ [player'] ++ drop (idx + 1) players
      where
        player = players !! idx
        player' = player {status = Bet bet}

-- This section defines exported functions used to inspect game state

getCombinations :: Poker -> Maybe [(String, Combination)]
getCombinations p =
  if isRoundEnd p
    then Just $ zip (map name $ players p) $ map (determineCombination (table p)) (players p)
    else Nothing
  where
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
    isRoundEnd :: Poker -> Bool
    isRoundEnd p' =
      let bets = map getBet $ filter isBet $ map status (players p')
       in all (uncurry (==)) $ zip bets (tail bets)
      where
        isBet :: Status -> Bool
        isBet (Bet _) = True
        isBet _ = False
        getBet :: Status -> Int
        getBet (Bet bet) = bet
        getBet _ = undefined

getWinner :: Poker -> Maybe String
getWinner poker =
  case getCombinations poker of
    Just cs -> Just $ fst $ maximumBy (\x y -> compare (snd x) (snd y)) cs
    Nothing -> name <$> getWinnerByFold poker
  where
    getWinnerByFold :: Poker -> Maybe Player
    getWinnerByFold p = case nonFolded of
      [] -> undefined
      [x] -> Just x
      _more -> Nothing
      where
        nonFolded = filter (\pl -> status pl /= Folded) (players p)

getPlays :: Poker -> [Play]
getPlays p = case status player of
  Init -> [Check, Open 0, AllIn (chips player)]
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

getRole :: Player -> Poker -> Role
getRole player poker = getRole' (fromJust $ findIndex (\pl -> name pl == name player) (players poker)) poker
  where
    getRole' 0 _ = SmallBlind
    getRole' 1 _ = BigBlind
    getRole' idx p =
      if (length (players p) > 2) && (idx == length (players p) - 1) then Dealer else None

getChips :: Player -> [Chip]
getChips player =
  concat
    [ replicate (num `div` 50) C50,
      replicate ((num `mod` 50) `div` 25) C25,
      replicate ((num `mod` 25) `div` 10) C10,
      replicate ((num `mod` 10) `div` 5) C5
    ]
  where
    num = chips player

-- This section defineds unexported utility functions

deal :: Int -> [Card] -> ([Card], [Card])
deal = splitAt

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

initGame :: Poker -> Maybe Poker
initGame p =
  case tryTakeBlind takeSmallBlind $ dealCards p of
    Nothing -> Nothing
    Just sb ->
      case tryTakeBlind takeBigBlind sb of
        Nothing -> Nothing
        Just bb ->
          if isPlayable bb
            then Just bb
            else Nothing
  where
    tryTakeBlind :: (Poker -> Maybe Poker) -> Poker -> Maybe Poker
    tryTakeBlind tb poker =
      case tb poker of
        Nothing -> tryTakeBlind tb $ poker {players = tail $ players poker}
        Just poker' -> Just poker'
    isPlayable :: Poker -> Bool
    isPlayable p' =
      length (players p') > 1 && chips (head $ players p') >= 5 && chips (head $ tail $ players p') >= 10
    dealCards :: Poker -> Poker
    dealCards p' =
      let (dealt, deck') = deal (2 * length (players p')) (deck p')
       in p' {deck = deck', players = dealToPlayers dealt (players p')}
      where
        dealToPlayers cards players = zipWith (\pl c -> pl {cards = c}) players (chunksOf 2 cards)
    takeSmallBlind :: Poker -> Maybe Poker
    takeSmallBlind p' =
      if chips blind >= 5
        then
          Just $
            p'
              { players = blind' : tail (players p'),
                bank = M.adjust (+ 5) 0 (bank p'),
                playerIdx = playerIdx p' + 1
              }
        else Nothing
      where
        blind = head (players p')
        blind' = blind {chips = chips blind - 5}
    takeBigBlind :: Poker -> Maybe Poker
    takeBigBlind p' =
      if chips blind >= 10
        then
          Just $
            p'
              { players = head (players p') : blind' : drop 2 (players p'),
                bank = M.adjust (+ 10) 1 (bank p'),
                playerIdx = playerIdx p' + 1
              }
        else Nothing
      where
        blind = head $ tail $ players p'
        blind' = blind {chips = chips blind - 10}

shuffle :: IO [Card]
shuffle = do
  RS.shuffle' initDeck initDeckSize <$> R.initStdGen
  where
    initDeckSize :: Int
    initDeckSize = 52
    initDeck :: [Card]
    initDeck = map (uncurry Card) $ liftA2 (,) suits ranks
      where
        suits = [Clubs, Spades, Hearts, Diamonds]
        ranks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

movePlayer :: Poker -> Poker
movePlayer p = p {playerIdx = (playerIdx p + 1) `mod` length (players p)}

skipFolded :: Poker -> Poker
skipFolded p = case status (players p !! playerIdx p) of
  Folded -> skipFolded $ movePlayer p
  _other -> p
