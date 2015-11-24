{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Open 
        ( -- * Remote Monad
          openPlist
        ) where

import Control.Concurrent
import Control.Exception

import Database.PlistBuddy.Types
import Database.PlistBuddy.Command

import System.Posix.Pty

import System.IO.Error (catchIOError)


handleIOErrors :: IO a -> IO a
handleIOErrors m =
  m `catchIOError` \ e -> throw $ PlistBuddyException $ "IO error, " ++ show e

openPlist :: FilePath -> IO Plist
openPlist fileName = handleIOErrors $ do
    (pty,ph) <- spawnWithPty
                    Nothing
                    False
                    "/usr/libexec/PlistBuddy"
                    ["-x",fileName]
                    (80,24)

    myWritePty pty "#\n" -- 1 times in 100, you need to poke the plist-buddy
    _ <- recvReply0 pty
    attr <- getTerminalAttributes pty
    setTerminalAttributes pty ((attr `withoutMode` EnableEcho) `withoutMode` ProcessInput) Immediately
    lock <- newMVar ()
    return $ Plist pty lock ph False ()

                