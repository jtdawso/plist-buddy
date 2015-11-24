{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Audit where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text(Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LB
import Database.PlistBuddy.Types

import System.IO
import System.Process
import System.Posix.Pty

import Data.Time

import GHC.Generics

import qualified Crypto.Hash.MD5 as MD5

{-
auditPlist :: FilePath -> Plist -> IO Plist
auditPlist auditFile (Plist {}) = do
  up <- 
-}

startAudit :: FilePath -> FilePath -> IO (Update -> IO ())
startAudit auditFile plistFile = do
  -- if there is no file, then this creates an empty file first
  au <- openBinaryFile auditFile WriteMode
  -- NOTE: for now, we worry about the writing part
  -- pretend the winding to the EOF has been done
  issue au =<< snapshot plistFile
--  au <- LB.openFile auditFile 
  return $ \ u -> do
    case u of
      Exit -> hClose au
      _ -> issue au $ Update u
  
snapshot :: FilePath -> IO Audit
snapshot file = do
  bs <- LB.readFile file
  hash <- return $! MD5.hashlazy bs
  return $ Hash $ B16.encode hash
  
issue :: Handle -> Audit -> IO ()
issue h u = do
  hPutStr h $ show u ++ " :\n"
  hFlush h
  
-- | Find the list of 'PlistBuddy' commands to recover the most recent version of the plist.
--   Be careful when running recover with the audit capability turned on; it can duplicate
--   the audit trail, because recovery is also write. (This should not break anything)
recover :: FilePath -> FilePath -> IO [Update]
recover auditFile plistFile = return []

