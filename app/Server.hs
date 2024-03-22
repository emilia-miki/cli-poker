{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader
import Data.Functor
import Data.Map qualified as M
import qualified Poker as P
import Web.Scotty.Trans hiding (status)
import Data.Aeson (ToJSON, decode)
import GHC.Generics (Generic)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BL

data AppState = AppState
  { users :: M.Map String Bool,
    poker :: Maybe P.Poker
  }

data PlayerView = PlayerView 
  { name :: String,
    chips :: Int,
    status :: P.Status
  } 
  deriving (Show, Generic)

data PokerView = PokerView
  { player :: P.Player,
    players :: [PlayerView],
    playerIdx :: Int,
    bank :: M.Map Int Int,
    table :: [P.Card]
  }
  deriving (Show, Generic)

data FullDisclosure = FullDisclosure
  { pokerView :: PokerView,
    combinations :: [P.Combination],
    winner :: String
  }
  deriving (Show, Generic)

fullDisclosure :: P.Poker -> FullDisclosure
fullDisclosure = undefined

getPlayerView :: P.Player -> PlayerView
getPlayerView p =
  PlayerView {
    name = P.name p,
    chips = P.chips p,
    status = P.status p
  }

getPokerView :: String -> P.Poker -> PokerView
getPokerView username p =
  PokerView {
    player = head $ filter (\ pl -> P.name pl == username) $ P.players p,
    players = map getPlayerView (P.players p),
    playerIdx = P.playerIdx p,
    bank = P.bank p,
    table = P.table p
  }

instance ToJSON PlayerView
instance ToJSON PokerView
instance ToJSON FullDisclosure

defaultAppState :: AppState
defaultAppState =
  AppState
    { users = M.empty,
      poker = Nothing
    }

newtype WebM a = WebM {runWebM :: ReaderT (TVar AppState) IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState), MonadUnliftIO)

webM :: (MonadTrans t) => WebM a -> t WebM a
webM = lift

gets :: (AppState -> b) -> WebM b
gets f = (ask >>= liftIO . readTVarIO) <&> f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
  sync <- newTVarIO defaultAppState
  -- 'runActionToIO' is called once per action.
  let runActionToIO m = runReaderT (runWebM m) sync

  scottyT 3000 runActionToIO app

-- This app doesn't use raise/rescue, so the exception
-- type is ambiguous. We can fix it by putting a type
-- annotation just about anywhere. In this case, we'll
-- just do it on the entire app.
app :: ScottyT WebM ()
app = do
  get "/register/:username" $ do
    username <- pathParam "username"
    webM $ modify $ \st -> st {users = M.insert username False (users st)}
    return ()
  post "/start/:username" $ do
    username <- pathParam "username"
    webM $ modify $ \st -> st {users = M.insert username True (users st)}
    users <- webM $ gets users
    when (all snd $ M.toList users) $ do
      poker <- liftIO $ P.initPoker $ map fst $ M.toList users
      webM $ modify $ \st -> st {users = M.empty, poker = Just poker}
  get "/game/:username" $ do
    -- username <- pathParam "username"
    poker <- webM $ gets poker
    case poker of
      Just p -> undefined
        -- if P.isWon p
        --   then do
        --     newP <- liftIO $ P.nextGame p
        --     webM $ modify (\ st -> st { poker = newP })
        --     json $ fullDisclosure p
        --   else
        --     if P.isRoundEnd p
        --       then do
        --         let newP = P.nextTurn p
        --         webM $ modify (\ st -> st { poker = Just newP })
        --         json $ getPokerView username newP
        --       else json $ getPokerView username p
      Nothing -> text "the game has not started yet"
    return ()
  get "/plays/:username" $ do
    username <- pathParam "username"
    poker <- webM $ gets poker
    case poker of
      Just p -> do
        let idx = fromJust $ findIndex (\pl -> P.name pl == username) (P.players p)
        if P.playerIdx p == idx
          then json $ P.getPlays p
          else text "it's not your turn yet"
      Nothing -> text "the game has not started yet"
  post "/play/:username" $ do
    username <- pathParam "username"
    poker <- webM $ gets poker
    body' <- bodyReader
    body'' <- liftIO body'
    case poker of
      Just p -> do
        let idx = fromJust $ findIndex (\pl -> P.name pl == username) (P.players p)
        if P.playerIdx p == idx
          then do
            let play = (decode $ BL.fromStrict body'') :: Maybe P.Play
            case play of
              Nothing -> text "invalid play"
              Just pl -> do
                webM $ modify (\ st -> st { poker = Just $ P.play pl p })
                return ()
          else text "it's not your turn yet"
      Nothing -> text "the game has not started yet"
    return ()
