{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace (trace)

import Data.Char (toLower)
import Data.List(isPrefixOf)
import Data.Maybe(isJust)

import Data.Text.Lazy (Text, unpack)

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

import CountdownGame.Game
import qualified CountdownGame.Rounds as Rounds
import qualified CountdownGame.Actions as Actions
import qualified CountdownGame.Cookies as Cookies

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

main :: IO ()
main = do
  state <- initState
  scotty 8080 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ do
      localHost <- Actions.isLocalhost
      if localHost
        then redirect "/admin"
        else redirect "/play"
    get "/play" $ Actions.play state
    get "/register" Actions.register
    get "/admin" $ Actions.admin state
    post "/register" $ Actions.postRegister state
    get "/api/players" $ Actions.getPlayers state
    get "/api/round" $ Actions.getRound state
