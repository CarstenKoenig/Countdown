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

import qualified CountdownGame.Views.Index
import qualified CountdownGame.Views.Play
import qualified CountdownGame.Views.Register
import qualified CountdownGame.Views.Admin
import qualified CountdownGame.Cookies as Cookies

main :: IO ()

main = scotty 8080 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" $ do
    localHost <- isLocalhost
    if localHost
      then redirect "/admin"
      else redirect "/play"
  get "/play" $ do
    registered <- isRegistered
    if not registered
      then redirect "/register"
      else do
           blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
           blaze CountdownGame.Views.Play.render
  get "/register" $ do
    blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
    blaze CountdownGame.Views.Register.render
  get "/admin" $ do
    localhost <- isLocalhost
    if not localhost
      then redirect "/admin"
      else do
         blaze $ link ! rel "stylesheet" ! href "styles.css" ! type_ "text/css"
         blaze CountdownGame.Views.Admin.render
  post "/register" $ do
    name <- param "nickName"
    let cookie = Cookies.PlayerCookie name 1
    Cookies.setPlayerCookie cookie
    redirect "/play"         

isLocalhost :: ActionM Bool
isLocalhost = do
  remote <- remoteHost <$> request
  return $ hostnameIsLocal remote
  where hostnameIsLocal sockAdr =
          "127.0.0.1" `isPrefixOf` show sockAdr ||
          "localhost" `isPrefixOf` (map toLower . show) sockAdr

isRegistered :: ActionM Bool
isRegistered = do
  player <- Cookies.getPlayerCookie
  trace (maybe "Cookie not set!" (unpack . Cookies.nickName) player) $ return $ isJust player

blaze :: Html -> ActionM ()
blaze = S.html . renderHtml
