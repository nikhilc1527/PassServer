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

type ServerData = Int
default_server_data :: ServerData
default_server_data = 0

string_to_bs :: String -> BSL.ByteString
string_to_bs = BSB.toLazyByteString . Data.Foldable.fold . map BSB.char7

main_handler :: IORef ServerData -> Server.Handler BSL.ByteString
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request 
  | length url_path > 0 = do
      file_exists <- Dir.doesFileExist url_path
      case file_exists of
        True -> do
          file <- BSL.fromStrict <$> BS.readFile url_path
          mime_type <- init <$> Proc.readProcess "xdg-mime" ["query", "filetype", url_path] ""
          return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType mime_type, Headers.mkHeader Headers.HdrContentLength $ show $ BSL.length file] file
        False -> do
          putStrLn $ "cannot access file " ++ url_path
          return $ Server.Response (4,0,4) "" [Headers.mkHeader Headers.HdrContentLength $ "0"] $ BSL.empty
  | True = do
      modifyIORef server_data (+ 1)
      serve_count <- readIORef server_data
      putStrLn $ show request
      let content_body = string_to_bs $ "<h1>" ++ (show serve_count) ++ "</h1>"
      let hdrs = [Headers.mkHeader Headers.HdrContentLength $ show $ BSL.length content_body, Headers.mkHeader Headers.HdrSetCookie "sample_cookie=hello; Max-Age=300"]
      return $ Server.Response (2,0,0) "" hdrs content_body

main_configuration :: Server.Config
main_configuration = Server.Config Log.stdLogger "localhost" 6969

main :: IO ()
main = do
  server_data <- newIORef default_server_data
  Server.serverWith main_configuration ((putStrLn . show) >> (main_handler server_data))
