module Main where

import Poker
import Text.Read (readMaybe)
import Control.Monad (replicateM)

playTurn :: Poker -> IO Poker
playTurn poker = do
  putStrLn ("it's " ++ name (players poker !! playerIdx poker) ++ "'s turn.")
  pl <- choosePlay (getPlays poker)
  return $ play pl poker

playRound :: Poker -> IO Poker
playRound poker = do
  p <- playTurn poker
  if isRoundEnd p
    then return p
    else playRound p

choosePlay :: [Play] -> IO Play
choosePlay plays = do
    putStrLn ("Choose your play: " ++ show plays)
    plStr <- getLine
    case words plStr of
      [] -> do
        putStrLn "No play entered"
        choosePlay plays
      [x] -> case x of
        "Fold" -> validate Fold
        "Check" -> validate Check
        other -> validError other
      [x, y] -> case x of
        "Open" -> do
          p <- readNum y Open
          validate p
        "Call" -> do
          p <- readNum y Call
          validate p
        "Raise" -> do
          p <- readNum y Raise
          validate p
        "AllIn" -> do
          p <- readNum y AllIn
          validate p
        other -> validError other
      other -> validError (unwords other)
  where validate p = case p of
          Fold ->
            if Fold `elem` plays
              then return Fold
              else valError Fold
          Check ->
            if Check `elem` plays
              then return Check
              else valError Check
          Open num ->
            if Open 0 `elem` plays
              then return $ Open num
              else valError (Open num)
          Call num ->
            if Call 0 `elem` plays
              then return $ Call num
              else valError (Call num)
          Raise num ->
            if Raise 0 `elem` plays
              then return $ Raise num
              else valError (Raise num)
          AllIn num ->
            if AllIn num `elem` plays
              then return $ AllIn num
              else valError (AllIn num)
        valError p = do
          putStrLn (show p ++ " is not a valid play in this case")
          choosePlay plays
        validError p = do
          putStrLn (p ++ " is not a valid play")
          choosePlay plays
        intError str = do
          putStrLn (str ++ " is not an integer")
          choosePlay plays
        readNum str p = case readMaybe str :: Maybe Int of
          Nothing -> intError str
          Just num ->
            if num == 0
            then do
              putStrLn (show (p num) ++ " is not a valid play: the bet can't be zero")
              choosePlay plays
            else return $ p num

playGame :: Poker -> IO Poker
playGame poker =
  if length (table poker) == 5
  then do
    p <- playRound poker
    putStrLn ("Combinations are " ++ show (getCombinations p))
    let winnerIdx = determineWinner p
    let winner = players p !! winnerIdx
    putStrLn (name winner ++ " has won and gets the bank")
    return $ giveBank winner p
  else do
    p <- playRound poker
    case getWinnerByFold p of
      Just winner -> do
        putStrLn (name winner ++ " has won and gets the bank")
        let p' = giveBank winner p
        return p'
      Nothing -> playGame (nextRound p)

playLoop :: Poker -> IO ()
playLoop p = do
    let p' = startGame p
    go p'
  where go p' = do
          played <- playGame p'
          if length (players played) == 1
            then do
              putStrLn (show (head $ players played) ++ " wins! Well Played!")
              putStrLn "Thank you for playing!"
              return ()
            else do
              next <- nextGame played
              case next of
                Nothing -> do
                  putStrLn "Thank you for playing!"
                  return ()
                Just next' -> go next'

main :: IO ()
main = do
  putStrLn "how many players do you have?"
  nStr <- getLine
  let n' = readMaybe nStr :: Maybe Int
  case n' of
    Nothing -> putStrLn (nStr ++ " is not a number!")
    Just n -> do
      putStrLn "enter the players' names, one per line:"
      names <- replicateM n getLine
      poker <- initPoker names
      playLoop poker
  return ()
