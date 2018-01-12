{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import DB
import Data.Aeson
import Data.String
import Data.Text
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sqlite
import Network.HTTP.Types
import Network.Wai (remoteHost)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import System.Environment
import Web.Scotty

data Input = Input { label :: String
                   , image :: String
                   , answer :: Bool
                   } deriving (Generic)

instance FromJSON Input

main :: IO ()
main = do
  [host, port, dbpath] <- getArgs
  ch <- newChan
  forkIO $ dbThread (pack dbpath) ch
  let opts = Options { verbose = 0
                     , settings = setHost (fromString host) $
                                  setPort (read port) $
                                  defaultSettings
                     }
  scottyOpts opts $ do
    post "/" $ do
      input <- jsonData
      hdrs <- headers
      remote <- fmap remoteHost request
      liftIO $ do time <- getCurrentTime
                  let row = Row time (show remote) hdrs (image input) (label input) (answer input)
                  writeChan ch row
      status ok200

dbThread :: Text -> Chan Row -> IO ()
dbThread path ch = do
  withSqlitePool path 10 $ \p -> do
    run p $ runMigration migrateAll
    let loop = do
          r <- readChan ch
          run p $ insert r
          loop
    loop
