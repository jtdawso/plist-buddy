{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy 
        ( -- * Remote Monad
          PlistBuddy()
        , openPlist
        , Plist()
        , send
        -- * The Remote Monad operators
        , help
        , exit
        , save
        , revert
        , clear
        , get
        , set
        , add
        , delete
        -- * Other types
        , TYPE(..)
        ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader

import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List ()
import Data.Monoid ((<>))

import System.Process
import System.IO
import System.Posix.Pty


------------------------------------------------------------------------------

-- | The Remote Monad
newtype PlistBuddy a = PlistBuddy (ReaderT Plist IO a)
  deriving (Functor,Applicative,Monad)

-- | The Remote Plist 
data Plist = Plist Pty ProcessHandle

send :: Plist -> PlistBuddy a -> IO a
send dev (PlistBuddy m) = runReaderT m dev

-- | Returns Help Text
help :: PlistBuddy Text
help = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty "Help"
        return $ T.filter (/= '\r') $ E.decodeUtf8 $ res

-- | Exits the program, changes are not saved to the file
exit :: PlistBuddy ()
exit = PlistBuddy $ do
        Plist pty ph <- ask
        liftIO $ do
            (void $ command pty "Exit") `catch` \ (e :: IOException) -> return ()
            waitForProcess ph
        return ()

-- | Saves the current changes to the file
save :: PlistBuddy ()
save = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty "Save"
        case res of
          "Saving..." -> return ()
          _ -> fail $ "save failed: " ++ show res


-- | Reloads the last saved version of the file
revert :: PlistBuddy ()
revert = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty "Revert"
        case res of
          "Reverting to last saved state..." -> return ()
          _ -> fail $ "revert failed: " ++ show res

-- | Clear Type - Clears out all existing entries, and creates root of Type
clear :: TYPE -> PlistBuddy ()
clear ty = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Clear " <> E.encodeUtf8 (typeText ty)
        case res of
          "Initializing Plist..." -> return ()
          _  -> fail $ "clear failed: " ++ show res

-- | Print Entry - Gets value of Entry.  Otherwise, gets file 
get :: [Text] -> PlistBuddy Text
get entry = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Print" <>  BS.concat [ ":" <> quote e | e <- entry ]
        -- idea: print in XML (-x flag), and decode in more detail
        return $ T.filter (/= '\r') $ E.decodeUtf8 res

-- | Set Entry Value - Sets the value at Entry to Value
set :: [Text] -> Text -> PlistBuddy ()
set entry value = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Set "  <> BS.concat [ ":" <> quote e | e <- entry ]
                                      <> " " <> quote value
        case res of
          "" -> return ()
          _  -> fail $ "set failed: " ++ show res
    
-- | Add Entry Type [Value] - Adds Entry to the plist, with value Value
add :: [Text] -> TYPE -> Maybe Text -> PlistBuddy ()
add entry ty optValue = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Add "  <> BS.concat [ ":" <> quote e | e <- entry ]
                                      <>  " " <> E.encodeUtf8 (typeText ty)
                                      <> maybe "" (\ v -> " " <> quote v) optValue
        case res of
          "" -> return ()
          _  -> fail $ "add failed: " ++ show res

-- | Delete Entry - Deletes Entry from the plist
delete :: [Text] -> PlistBuddy ()
delete entry = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "delete " <>  BS.concat [ ":" <> quote e | e <- entry ]
        case res of
          "" -> return ()
          _  -> fail $ "delete failed: " ++ show res

{-                
-- Not (yet) supported
--    Copy <EntrySrc> <EntryDst> - Copies the EntrySrc property to EntryDst
--    Merge <file.plist> [<Entry>] - Adds the contents of file.plist to Entry
--    Import <Entry> <file> - Creates or sets Entry the contents of file
-}

quote :: Text -> ByteString
quote q = "'" <> BS.concatMap esc (E.encodeUtf8 q) <> "'"
  where esc 39 = "\\'"
        esc 92 = "\\\\"  -- RTT moment
        esc 10 = "\\n"
        esc c  = BS.pack [c]
        
------------------------------------------------------------------------------

data TYPE = STRING | ARRAY | DICT | BOOL | REAL | INTEGER | DATE | DATA
        deriving (Eq, Ord)

typeText:: TYPE -> Text
typeText STRING = "string"
typeText ARRAY  = "array"
typeText DICT   = "dict"
typeText BOOL   = "bool"
typeText REAL   = "real"
typeText INTEGER = "integer"
typeText DATE   = "date"
typeText DATA   = "data"

------------------------------------------------------------------------------

openPlist :: FilePath -> IO Plist
openPlist fileName = do
    (pty,ph) <- spawnWithPty
                    Nothing
                    False
                    "/usr/libexec/PlistBuddy"
                    [fileName]
                    (80,24)
    attr <- getTerminalAttributes pty
    setTerminalAttributes pty (attr `withoutMode` EnableEcho) Immediately
    _ <- recvReply pty 
    return $ Plist pty ph

command :: Pty -> ByteString -> IO ByteString
command pty input = do
--        print input
        when (not $ BS.null input) $  writePty pty input -- quirk of pty's?
        writePty pty "\n"
        recvReply pty 

recvReply :: Pty -> IO ByteString
recvReply pty = readMe []
  where
    prompt = "\r\nCommand: "

    readMe rbs = do
            t <- readPty pty
--            print t
            testMe (t : rbs)

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


