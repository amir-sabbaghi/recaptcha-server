{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Control.Monad.IO.Class
import DB
import Data.Aeson
import Data.Text
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Sqlite
import Network.HTTP.Types
import System.Environment
import Web.Scotty

data Input = Input { label :: String
                   , image :: String
                   , answer :: Bool
                   } deriving (Generic)

instance FromJSON Input

main :: IO ()
main = do
  [port, dbpath] <- getArgs
  withSqlitePool (pack dbpath) 10 $ \p -> do
    run p $ runMigration migrateAll
    scotty (read port) $ do
      post "/" $ do
        input <- jsonData
        hdrs <- headers
        liftIO $ do time <- getCurrentTime
                    let row = Row time "" hdrs (image input) (label input) (answer input)
                    run p $ insert row
        status ok200
