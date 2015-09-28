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

import Network.Socket (SockAddr(..))
import Network.Wai (remoteHost)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)

import qualified CountdownGame.Actions as Actions
import qualified CountdownGame.Cookies as Cookies

main :: IO ()

main = scotty 8080 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" $ do
    localHost <- Actions.isLocalhost
    if localHost
      then redirect "/admin"
      else redirect "/play"
  get "/play" Actions.play
  get "/register" Actions.register
  get "/admin" Actions.admin
  post "/register" Actions.postRegister
