module Main where

import qualified Network.HTTP.Server as Server
import qualified Network.Socket.Internal as Socket
import qualified Network.URL as URL
import qualified Network.HTTP.Server.Logger as Log
import Network.HTTP.Headers as Headers
import Data.IORef
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.Random as Rand
import qualified Data.Time.Clock as Clock
import Data.Fixed
import Data.Void
import Control.Concurrent
import qualified System.Process as Proc
import qualified Text.Megaparsec as Parse
import System.IO
import System.Exit
import Data.Maybe

type SessionID = (String, String)

max_session_time_secs :: Pico
max_session_time_secs = 20
max_session_time :: Clock.NominalDiffTime
max_session_time = Clock.secondsToNominalDiffTime max_session_time_secs

data ServerData = ServerData { logged_in :: Map.Map String String, passwords :: Map.Map String String } deriving (Show, Eq)

default_server_data :: ServerData
default_server_data = ServerData { logged_in=Map.empty, passwords=Map.empty }

gen_new_sessionid :: IO SessionID
gen_new_sessionid = do
  let len = 20
  std_gen <- Rand.getStdGen
  let id = take len $ Rand.randomRs ('a', 'z') std_gen -- get <len> random characters between a-z
  return $ (id, "")

type Parser = Parse.Parsec Void String

read_passwords :: String -> Map.Map String String
read_passwords input = Map.fromList passwords_list
  where
    parser :: Parser [(String, String)] = Parse.many $ do
      username <- Parse.manyTill (Parse.anySingle) (Parse.single ':')
      password <- Parse.manyTill (Parse.anySingle) (Parse.single '\n')
      return $ (username, password)
    Right passwords_list = Parse.parse parser "" input

read_password_file :: FilePath -> IO (Map.Map String String)
read_password_file in_file = do
  input <- readFile in_file
  return $ read_passwords input

print_passwords :: Map.Map String String -> String
print_passwords = concat . map (\(x, y) -> x ++ ":" ++ y ++ "\n") . Map.toList

output_passwords_to_file :: FilePath -> String -> Map.Map String String -> IO ()
output_passwords_to_file filepath pass passwords_map = do
  writeFile filepath $ print_passwords passwords_map

-- password -> maybe the list of passwords
-- if the given password is wrong then you cant get the passwords from the gpg file
check_decrypt :: String -> IO (Maybe String)
check_decrypt password = do
  let proc = (Proc.proc "gpg" $ ["--batch", "--yes", "--passphrase", password, "-d", "passwords.gpg"]) { Proc.std_out = Proc.CreatePipe }
  (stdin, stdout_mb, stderr, proc_handle) <- Proc.createProcess proc
  exit_code <- Proc.waitForProcess proc_handle
  case exit_code of
    ExitSuccess -> do
      let stdout = fromJust stdout_mb
      passwords <- hGetContents stdout
      return $ Just passwords
    ExitFailure _ -> do
      return $ Nothing

main_handler :: IORef ServerData -> Server.Handler String
-- GET
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request@(Server.Request rq_uri Server.GET rq_headers rq_body) = do
  let cookie_headers = filter (\hdr -> case hdr of
                                         Header HdrCookie _ -> True
                                         _ -> False) rq_headers
  case length cookie_headers of
    0 -> do
      file <- readFile "index.html"
      sessid@(id, pass) <- gen_new_sessionid
      return $ Server.Response (2,0,0) "" [mkHeader HdrContentType "text/html", mkHeader HdrContentLength $ show $ length file, mkHeader HdrSetCookie $ "id=" ++ id] file
    _ -> do
      let (Header HdrCookie sessid_long) = head cookie_headers
      let sessid = drop 3 $ sessid_long
      srv_data <- readIORef server_data
      case Map.lookup sessid $ logged_in srv_data of
        Just master_password -> do -- LOGGED IN
          Just passwords_str <- check_decrypt master_password
          let passmap = read_passwords passwords_str
          let pass_html = "<p>\n" ++ (concat $ map process_line $ Map.toList passmap) ++ "</p>"
          success <- fmap (++ (pass_html)) $ readFile "index_success.html"
          return $ Server.Response (2,0,0) "" [mkHeader HdrContentType "text/html", mkHeader HdrContentLength $ show $ length success] success -- need to make unique identifiers for each client with individual expiry times
        Nothing -> do
          file <- readFile "index.html"
          return $ Server.Response (2,0,0) "" [mkHeader HdrContentType "text/html", mkHeader HdrContentLength $ show $ length file] file
  where
    process_line :: (String, String) -> String
    process_line (username, pass) = "<a href=\"/\" onclick=\"func(&quot;" ++ pass ++ "&quot;); return true;\">" ++ username ++ "</a><br>"

-- POST
main_handler server_data sockaddr url@(URL.URL url_type url_path url_params) request@(Server.Request rq_uri Server.POST rq_headers rq_body) = do
  let parser :: Parser (String, String) = do
       Parse.manyTill (Parse.anySingle) (Parse.single '=')
       username <- Parse.manyTill (Parse.anySingle) (Parse.single '&')
       Parse.manyTill (Parse.anySingle) (Parse.single '=')
       password <- Parse.takeRest
       return $ (username, password)

  let Right (username, password) = Parse.parse parser "" rq_body

  -- hard coding the username and password (for now)
  decrypt_res <- check_decrypt password

  case decrypt_res of
    Just _ -> do
      let Header HdrCookie sessid_long = head $ filter (\hdr -> case hdr of
                                                           Header HdrCookie _ -> True
                                                           _ -> False) rq_headers
      let sessid = drop 3 $ sessid_long
      modifyIORef server_data $ \(ServerData log passwords) -> ServerData (Map.insert sessid password log) passwords
      forkIO $ do
        threadDelay $ (fromEnum $ max_session_time_secs) `div` (10 ^ 6)
        atomicModifyIORef server_data $ \(ServerData logged passwords) -> (ServerData (Map.delete sessid logged) passwords, ())
      return $ Server.Response (3,0,1) "" [mkHeader HdrContentLength "0", mkHeader HdrLocation "/"] ""
    Nothing -> do
      failure <- readFile "index_failure.html"
      return $ Server.Response (2,0,0) "" [mkHeader HdrContentType "text/html", mkHeader HdrContentLength $ show $ length failure] failure

main_configuration :: Server.Config
main_configuration = Server.Config Log.stdLogger "localhost" 6969

main :: IO ()
main = do
  server_data <- newIORef $ default_server_data
  Server.serverWith main_configuration (main_handler server_data)
