{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Command
        ( command
        , recvReply0
        , myWritePty
        )
        where

import Control.Exception
import Control.Monad

import Data.Char (ord)
import Database.PlistBuddy.Types

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Monoid ((<>))

import System.Posix.Pty
import System.Timeout

-- Single threaded; assumes a lock above it.
command :: Plist -> ByteString -> IO ByteString
command (Plist pty _ _ d) input = todo
  where
    write txt = do
      when d $ print ("write"::String,txt)
      myWritePty pty txt

    todo = do
        write (input <> "\n")
        r <- recvReply pty
        when d $ print ("read"::String,r)
        return $ r


recvReply0 :: Pty -> IO ByteString
recvReply0 pty = readMe []
  where
    readMe rbs = do
            v <- myReadPty pty
            testMe ( BS.filter (/= fromIntegral (ord '\r')) v : rbs)

    testMe rbs | "#\nCommand: Unrecognized Command\nCommand: " == bs
               = return $ ""
               | "Command: #\nUnrecognized Command\nCommand: " == bs
               = return $ ""
               | otherwise
               = readMe rbs
      where
              bs = rbsToByteString rbs

recvReply :: Pty -> IO ByteString
recvReply pty = readMe []
  where
    prompt = "\nCommand: "

    readMe rbs = do
            v <- myReadPty pty
            testMe ( BS.filter (/= fromIntegral (ord '\r')) v : rbs)

    testMe rbs | prompt `isSuffixOf` rbs
               = return $ BS.take (BS.length bs - BS.length prompt) bs
               | "Command: " == bs
               = return $ ""
               | otherwise
               = readMe rbs
      where
              bs = rbsToByteString rbs

type RBS = [ByteString] -- reversed list of strict bytestring

rbsToByteString :: RBS -> ByteString
rbsToByteString = BS.concat . reverse
          
isSuffixOf :: ByteString -> RBS -> Bool
isSuffixOf bs rbs = bs `BS.isSuffixOf` rbsToByteString rbs

myWritePty :: Pty -> ByteString -> IO ()
myWritePty pty msg = do
  r <- myTimeout $ do
      threadWaitWritePty pty
      writePty pty msg
  case r of
    Just () -> return ()
    Nothing -> do
      throw $ PlistBuddyException "timeout when writing"

myReadPty :: Pty -> IO ByteString
myReadPty pty = do
  r <- myTimeout $ do
      threadWaitReadPty pty
      tryReadPty pty
  case r of
    Just (Left {}) -> myReadPty pty
    Just (Right v) -> return v
    Nothing        -> do
      throw $ PlistBuddyException "timeout when reading"

myTimeout :: IO a -> IO (Maybe a)
myTimeout = timeout (1000 * 1000)

