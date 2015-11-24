{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Audit where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text(Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Database.PlistBuddy.Types

import System.IO
import System.Process
import System.Posix.Pty

import Data.Time

import GHC.Generics

import qualified Crypto.Hash.MD5 as MD5


startAudit :: FilePath -> FilePath -> IO (Write -> IO ())
startAudit auditFile plistFile = do
  return $ undefined
  
snapshot :: FilePath -> IO ByteString
snapshot file = do
  bs <- LB.readFile file
  return $! MD5.hashlazy bs
  
issue :: Handle -> Transaction -> IO ()
issue h t = return ()

-- | Find the list of 'PlistBuddy' commands to recover the most recent version of the plist.
--   Be careful when running recover with the audit capability turned on; it can duplicate
--   the audit trail, because recovery is also write. (This should not break anything)
recover :: FilePath -> FilePath -> IO (PlistBuddy ())
recover auditFile plistFile = return (return ())
