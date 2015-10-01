{-# LANGUAGE OverloadedStrings    #-}

module CountdownGame.Database
       ( initializeDatabase
       , addPlayer
       , updatePlayer
       , getPlayer
       , checkPlayer
       , getPlayers
       , getPlayersMap
       , insertChallange
       , setPlayerScore
       ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Map.Strict as M
import Database.Persist
import Database.Persist.Sql (insert, fromSqlKey, toSqlKey)
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH

import CountdownGame.Database.Models
import qualified Countdown.Game as G
import qualified Countdown.Game.Players as P

connectionString :: Text
connectionString = "countdown.db"

initializeDatabase :: IO ()
initializeDatabase = runSqlite connectionString $ do runMigration migrateAll

addPlayer :: Text -> IO P.Player
addPlayer nick = runSqlite connectionString $ addPlayer' nick

getPlayer :: P.PlayerId -> IO (Maybe P.Player)
getPlayer id = runSqlite connectionString $ do
  pl <- get (toSqlKey $ fromIntegral id)
  return $ case pl of
    Nothing -> Nothing
    Just p  -> Just $ P.Player (playerNickname p) id

checkPlayer :: P.PlayerId -> Text -> IO (Maybe P.Player)
checkPlayer id nick = runSqlite connectionString $ do
  pl <- get (toSqlKey $ fromIntegral id)
  return $ case pl of
    Nothing -> Nothing
    Just p  ->
      let nick' = playerNickname p
      in if nick' == nick
         then Just $ P.Player (playerNickname p) id
         else Nothing

updatePlayer :: P.PlayerId -> Text -> IO P.Player
updatePlayer id nick = runSqlite connectionString $ do
  let pId = toSqlKey $ fromIntegral id
  pl <- get pId
  case pl of
    Nothing -> addPlayer' nick
    Just p  -> do
      replace pId $ Player nick
      return $ P.Player nick id

getPlayersMap :: IO P.PlayersMap
getPlayersMap = do
  ps <- map (\ p -> (P.playerId p, p)) <$> getPlayers
  return $ M.fromList ps

getPlayers :: IO [P.Player]
getPlayers = runSqlite connectionString $ do
  pls <- selectList [] []
  return $ map fromEntity pls
  where
    fromEntity entity =
       P.Player (playerNickname $ entityVal entity) (fromIntegral . fromSqlKey $ entityKey entity)

addPlayer' nick = do
  key <- insert $ Player nick
  let id = fromIntegral $ fromSqlKey key
  return $ P.Player nick id

insertChallange :: G.Challange -> IO Int64
insertChallange ch = runSqlite connectionString $ do
  key <- insert $ Challange (G.targetNumber ch) (G.availableNumbers ch)
  return $ fromSqlKey key

setPlayerScore :: Int64 -> P.PlayerId -> Int -> IO ()
setPlayerScore chId pId score = runSqlite connectionString $ do
  let id = toSqlKey chId
      pid = toSqlKey $ fromIntegral pId
  sc <- getBy $ Index pid id
  case sc of
    Nothing -> insert_ $ Score score pid id
    Just e  -> update (entityKey e) [ ScoreScore =. score ]
      
