{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Audit ( auditOn, snapshot ) where

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

auditOn :: FilePath -> Plist -> IO Plist
auditOn auditFile plist = do
  up <- startAudit auditFile (plist_file plist)
  return $ plist { plist_trail = up }

startAudit :: FilePath -> FilePath -> IO (Trail -> IO ())
startAudit auditFile plistFile = do
  -- if there is no file, then this creates an empty file first
  au <- openBinaryFile auditFile AppendMode
  -- state what we are auditing, with blank line(s)
  -- in case the previous line was incomplete
  hPutStr au $ "\n\n" ++ take 72 (cycle "-") ++ "\n" 
  t <- getCurrentTime
  h <- snapshot plistFile
  issue au $ t :! h
  -- and append to the audit file
  return $ \ u -> do
    t <- getCurrentTime
    issue au $ t :! u
    case u of
      Exit -> hClose au
      _     -> return ()

snapshot :: FilePath -> IO Trail
snapshot file = do
  bs <- LB.readFile file
  return $! Hash $! B16.encode $! MD5.hashlazy bs
  
issue :: Show a => Handle -> a -> IO ()
issue h u = do
  hPutStr h $ show u ++ "\n"
  hFlush h
  
-- | Find the list of 'PlistBuddy' commands to recover the most recent version of the plist.
--   Be careful when running recover with the audit capability turned on; it can duplicate
--   the audit trail, because recovery is also write. (This should not break anything)
recover :: FilePath -> FilePath -> IO [Trail]
recover auditFile plistFile = return []

