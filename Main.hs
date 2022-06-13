module Main where

import qualified Network.HTTP.Server as Server
import qualified Network.Socket.Internal as Socket
import qualified Network.URL as URL
import qualified Network.HTTP.Server.Logger as Log
import qualified Network.HTTP.Headers as Headers
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import qualified Data.ByteString.Builder as BSB
import qualified System.Directory as Dir
import qualified System.Process as Proc
import qualified Text.Blaze.Html5 as HTML

import qualified Data.Attoparsec.ByteString as Atto

type ServerData = Int
default_server_data :: ServerData
default_server_data = 0

get_until :: Char -> String -> (String, String)
get_until delim str
  | length str == 0 = ([], [])
  | head str == delim = ([], tail str)
  | True = let (a, b) = get_until delim $ tail str in ((head str):a, b)      

main_handler :: IORef ServerData -> Server.Handler String
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request@(Server.Request rq_uri rq_method rq_headers rq_body) = 
  case rq_method of
    Server.GET -> do
      putStrLn $ show request
      file <- readFile "index.html"
      return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length file] file
    Server.POST -> do -- request has to be of verifying username and password
      let (_, r1) = get_until '=' rq_body
      let (username, r2) = get_until '&' r1
      let (_, r3) = get_until '=' r2
      let password = r3

      -- hard coding the username and password
      case (username == "nikhilc" && password == "password") of
        True -> do
          success <- readFile "index_success.html"
          return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length success, Headers.mkHeader Headers.HdrSetCookie "logged_in=true; Max-Age=10"] success -- need to make unique identifiers for each client with individual expiry times
        False -> do
          failure <- readFile "index_failure.html"
          return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length failure] failure

main_configuration :: Server.Config
main_configuration = Server.Config Log.stdLogger "localhost" 6969

main :: IO ()
main = do
  server_data <- newIORef default_server_data
  Server.serverWith main_configuration (main_handler server_data)
