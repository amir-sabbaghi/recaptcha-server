{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Control.Monad.IO.Class
import Data.Aeson
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
  [port] <- getArgs
  scotty (read port) $ do
    post "/" $ do
      input <- jsonData
      liftIO $ print $ label input
      status ok200
