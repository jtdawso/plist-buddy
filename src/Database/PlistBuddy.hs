{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Database.PlistBuddy where

import Control.Monad
import Control.Monad.Reader

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List ()
import Data.Monoid ((<>))

import System.Process
import System.IO
import Control.Concurrent
import System.Posix.Pty

------------------------------------------------------------------------------

spawnPlistBuddy :: FilePath -> IO (Pty,ProcessHandle)
spawnPlistBuddy fileName = do
    (pty,ph) <- spawnWithPty
                    Nothing
                    False
                    "/usr/libexec/PlistBuddy"
                    [fileName]
                    (80,24)
    attr <- getTerminalAttributes pty
    setTerminalAttributes pty (attr `withoutMode` EnableEcho) Immediately
    _ <- recvReply pty ["\r\n"]
    return (pty,ph)        

command :: Pty -> ByteString -> IO ByteString
command pty input = do
        when (not $ BS.null input) $  writePty pty input -- quirk of pty's?
        writePty pty "\n"
        recvReply pty []

recvReply :: Pty -> RBS -> IO ByteString
recvReply pty rbs = do
            t <- readPty pty
            print t
            testMe (t : rbs)
  where
    prompt = "\r\nCommand: "

    testMe rbs | prompt `isSuffixOf` rbs
               = let bs = rbsToByteString rbs 
                 in return $ BS.take (BS.length bs - BS.length prompt) bs
               | otherwise
               = recvReply pty rbs

type RBS = [ByteString] -- reversed list of strict bytestring

rbsToByteString :: RBS -> ByteString
rbsToByteString = BS.concat . reverse
          
isSuffixOf :: ByteString -> RBS -> Bool
isSuffixOf bs rbs = bs `BS.isSuffixOf` rbsToByteString rbs
