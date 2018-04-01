{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Database.PlistBuddy.Command
        ( command
        , recvReply0
        , myWritePty
        )
        where

import           Control.Exception
import           Control.Monad

import           Data.Char                 (ord)
import           Data.Word                 (Word8)
import           Database.PlistBuddy.Types

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.Monoid               ((<>))

import           System.Posix.Pty
import           System.Timeout

-- Single threaded; assumes a lock above it.
command :: Plist -> ByteString -> IO ByteString
command plist input = todo
  where
    d = plist_debug plist
    pty = plist_pty plist
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
            testMe ( BS.filter (`notElem` toIgnore) v : rbs)

    toIgnore :: [Word8]
    toIgnore = BS.unpack "\r\n#"

    testMe rbs | "Command: Unrecognized CommandCommand: " `isSuffixOf` rbs
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
               = return ""
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
    Nothing -> throw $ PlistBuddyException "timeout when writing"

myReadPty :: Pty -> IO ByteString
myReadPty pty = do
  r <- myTimeout $ do
      threadWaitReadPty pty
      tryReadPty pty
  case r of
    Just (Left {}) -> myReadPty pty
    Just (Right v) -> return v
    Nothing        -> throw $ PlistBuddyException "timeout when reading"

myTimeout :: IO a -> IO (Maybe a)
myTimeout = timeout (1000 * 1000)
