module Main where

import qualified Network.HTTP.Server as Server
import qualified Network.Socket.Internal as Socket
import qualified Network.URL as URL
import qualified Network.HTTP.Server.Logger as Log
import qualified Network.HTTP.Headers as Headers
import Data.IORef
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as Rand
import qualified Data.Time.Clock as Clock
import Data.Fixed

type SessionID = (String, Clock.UTCTime)

max_session_time_secs :: Pico
max_session_time_secs = 60
max_session_time :: Clock.NominalDiffTime
max_session_time = Clock.secondsToNominalDiffTime max_session_time_secs

data ServerData = 
  ServerData 
    {
      -- unlogged_in :: Set.Set String,
      logged_in :: Map.Map String Clock.UTCTime
    } deriving (Show, Eq)

default_server_data :: ServerData
default_server_data = ServerData 
  { 
    -- unlogged_in=Set.empty, 
    logged_in=Map.empty 
  }

gen_new_sessionid :: IO SessionID
gen_new_sessionid = do
  let len = 20
  std_gen <- Rand.getStdGen
  let id = take len $ Rand.randomRs ('a', 'z') std_gen -- get <len> random characters between a-z
  curtime <- Clock.getCurrentTime
  return $ (id, curtime)

get_until :: Char -> String -> (String, String)
get_until delim str
  | length str == 0 = ([], [])
  | head str == delim = ([], tail str)
  | True = let (a, b) = get_until delim $ tail str in ((head str):a, b)      

main_handler :: IORef ServerData -> Server.Handler String
-- GET
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request@(Server.Request rq_uri Server.GET rq_headers rq_body) = do
  let cookie_headers = filter (\hdr -> case hdr of
                                         Headers.Header Headers.HdrCookie _ -> True
                                         _ -> False) rq_headers
  case length cookie_headers of
    0 -> do
      file <- readFile "index.html"
      sessid@(id, gentime) <- gen_new_sessionid
      -- modifyIORef server_data $ \(ServerData unlog log) -> let new_unlog = Set.insert id unlog in ServerData new_unlog log
      return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length file, Headers.mkHeader Headers.HdrSetCookie $ "id=" ++ id] file
    _ -> do
      let (Headers.Header Headers.HdrCookie sessid_long) = head cookie_headers
      let sessid = drop 3 $ sessid_long
      srv_data <- readIORef server_data
      case Map.lookup sessid $ logged_in srv_data of
        Just _ -> do
          success <- readFile "index_success.html"
          return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length success] success -- need to make unique identifiers for each client with individual expiry times
        Nothing -> do
          file <- readFile "index.html"
          -- modifyIORef server_data $ \(ServerData unlog log) -> ServerData (Set.insert sessid unlog) log
          return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length file] file

-- POSt
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request@(Server.Request rq_uri Server.POST rq_headers rq_body) = do
  let (_, r1) = get_until '=' rq_body
  let (username, r2) = get_until '&' r1
  let (_, r3) = get_until '=' r2
  let password = r3

  -- hard coding the username and password
  case (username == "nikhilc" && password == "password") of
    True -> do
      let Headers.Header Headers.HdrCookie sessid_long = head $ filter (\hdr -> case hdr of
                                                           Headers.Header Headers.HdrCookie _ -> True
                                                           _ -> False) rq_headers
      let sessid = drop 3 $ sessid_long
      success <- readFile "index_success.html"
      curtime <- Clock.getCurrentTime
      -- modifyIORef server_data $ \(ServerData unlog log) -> ServerData (Set.delete sessid unlog) (Map.insert sessid curtime log)   
      modifyIORef server_data $ \(ServerData log) -> ServerData $ Map.insert sessid curtime log
      return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length success] success -- need to make unique identifiers for each client with individual expiry times
    False -> do
      failure <- readFile "index_failure.html"
      return $ Server.Response (2,0,0) "" [Headers.mkHeader Headers.HdrContentType "text/html", Headers.mkHeader Headers.HdrContentLength $ show $ length failure] failure

main_configuration :: Server.Config
main_configuration = Server.Config Log.stdLogger "localhost" 6969

main :: IO ()
main = do
  server_data <- newIORef default_server_data
  Server.serverWith main_configuration (main_handler server_data)
