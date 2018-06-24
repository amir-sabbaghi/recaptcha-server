{-# LANGUAGE OverloadedStrings #-}

import DB
import Data.Text
import Database.Persist.Sqlite
import System.Environment
import System.Directory
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  [dbpath] <- getArgs
  extractDB $ pack dbpath

extractDB :: Text -> IO ()
extractDB path = do
  withSqlitePool path 10 $ \p -> do
    run p $ runMigration migrateAll
    list <- run p $ selectList [] []
    mapM_ dump list
    where dump e = do
            let r = entityVal e
                rId = entityKey e
                folder = "extract/" ++ rowLabel r ++ "/" ++ show (rowAnswer r)
                file = folder ++ "/" ++ show (fromSqlKey rId) ++ ".png"
            createDirectoryIfMissing True folder
            BS.writeFile file $ toBinary $ BS.pack $ rowImage r
          toBinary s = let prefix = "data:image/png;base64,"
                       in case BS.isPrefixOf prefix s of
                            True -> (\(Right a) -> a) $ BS.decode $ BS.drop (BS.length prefix) s
