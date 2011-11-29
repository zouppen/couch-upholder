{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Console.CmdArgs.Implicit
import DbHelpers
import Database.CouchDB
import Text.JSON

data Args = Args { server_   :: String
                 , database_ :: String
                 , design_   :: String
                 , view_     :: String
                 } deriving (Show, Data, Typeable)

synopsis =
  Args{ server_ = defDB &= typ "URL" &= help 
                  ("CouchDB server URL (default: " ++ defDB ++ ")")
      , database_ = def &= name "b" &= typ "STRING" &= help "Database name"
      , design_ = def &= typ "STRING" &= help "Design document"
      , view_ = def &= typ "STRING" &= help "Migration view"
      }
  &= program "uphold"
  &= summary "CouchDB upholder 0.1"
  &= help "CouchDB equivalent of ALTER TABLE. New database \"schema\" is created as a view and this tool is used to actually modify the database."

main = do
  args <- cmdArgs synopsis
  let nonempty = \ field msg -> when (""==field args) $ error msg
  nonempty database_ "Database name is required"
  nonempty design_ "Design document name is required"
  nonempty view_ "Migration view is required"
  -- Check DWLs and run
  runCouchDBURI (toURI $ server_ args) (run args)

run :: Args -> CouchMonad ()
run Args{..} = do
  docs <- queryView (db database_) (doc design_) (doc view_) []
  mapM_ update docs
  liftIO $ putStrLn "Complete!"
  where 
    update (doc,new) = do
      -- Using newDoc because the docs already contain revision and ID
      liftIO $ putStrLn $ "Updating " ++ show doc
      newDoc (db database_) (new :: JSValue)
