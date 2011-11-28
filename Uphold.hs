{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (when)
import System.Console.CmdArgs.Implicit
import DbHelpers

data Args = Args { server_   :: String
                 , database_ :: FilePath
                 , view_     :: FilePath
                 } deriving (Show, Data, Typeable)

synopsis =
  Args{ server_ = defDB &= typ "URL" &= help 
                  ("CouchDB server URL (default: " ++ defDB ++ ")")
      , database_ = def &= typ "DB" &= help "Database name"
      , view_ = def &= typ "VIEW" &= help "Migration view"
      }
  &= program "uphold"
  &= summary "CouchDB upholder 0.1"
  &= help "CouchDB equivalent of ALTER TABLE. New database \"schema\" is created as a view and this tool is used to actually modify the database."

main = do
  args <- cmdArgs synopsis
  when (""==database_ args) $ error "Database name is required"
  when (""==view_ args) $ error "Migration view is required"
  -- Check DWLs and run
  print args
