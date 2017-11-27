{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Lazy
import Data.Time.Clock
import Database.Persist.Sql
import Database.Persist.TH
import System.Log.FastLogger
import System.Log.Logger

type Headers = [(Text, Text)]

run e p = liftIO $ runSqlPersistMPool p e

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Row
  time UTCTime
  source String
  headers Headers
  image String
  label String
  answer Bool
|]

instance MonadLogger IO where
  monadLoggerLog _ source lvl msg = debugM (T.unpack source) (BS.unpack $ fromLogStr $ toLogStr msg)
