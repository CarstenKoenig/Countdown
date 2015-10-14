{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace (trace)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isJust)
import Data.Default.Class (def)
import Data.String (fromString)
import Data.Text (Text, unpack, pack)

import Web.Scotty
import qualified Web.Scotty as S

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Text.Blaze.Html5 (link, (!))
import Text.Blaze.Html5.Attributes

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Network.Wai.Handler.Warp (setHost, setPort, defaultSettings)

import System.Environment (getArgs, lookupEnv)
import System.FilePath (combine)

import qualified CountdownGame.Actions as Actions
import qualified CountdownGame.Cookies as Cookies

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

import CountdownGame.Spiel (initState, connectionPool)
import CountdownGame.Database (initializeDatabase)

main :: IO ()
main = do
  opts <- commandLineOptions
  baseDir <- baseFolder
  dataDir <- dataFolder
  let static = baseDir `combine` "static"
      dbFile = dataDir `combine` "countdown.db"
  putStrLn $ "serving " ++ static ++ " from " ++ dbFile
  
  state <- initState (pack dbFile) 4
  initializeDatabase (connectionPool state)
  scottyOpts opts $ do
    -- middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase static)
    get "/" $ do
      localHost <- Actions.isLocalhost
      if localHost
        then redirect "/admin"
        else redirect "/play"
    get "/anleitung" $ Actions.anleitung
    get "/play" $ Actions.play state
    get "/register" Actions.register
    get "/admin" $ Actions.admin state
    get "/scores" $ Actions.highScores state
    post "/register" $ Actions.postRegister state
    get "/api/players" $ Actions.getPlayers state
    get "/api/current" $ Actions.getSnapshot state
    get "/api/eval/:formula" $ Actions.evalFormula state
    
-- | reads scotty-options from the command-line arguments
-- expects at least two arguments: first the IP to be used followed by the port
commandLineOptions :: IO Options
commandLineOptions = do
  (ip:port:_) <- getArgs
  let sets = setPort (read port) . setHost (fromString ip) $ defaultSettings
  return $ def { verbose = 0, settings = sets }

baseFolder :: IO FilePath
baseFolder = do
    maybe "." Prelude.id <$>  lookupEnv repoDirEnvName

dataFolder :: IO FilePath
dataFolder = do
    maybe "." Prelude.id <$>  lookupEnv dataDirEnvName


repoDirEnvName :: String
repoDirEnvName = "OPENSHIFT_REPO_DIR"

dataDirEnvName :: String
dataDirEnvName = "OPENSHIFT_DATA_DIR"
