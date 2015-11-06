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
        , Value(..)
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

import Text.XML.Light

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
get :: [Text] -> PlistBuddy (Maybe Value)
get entry = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Print" <>  BS.concat [ ":" <> quote e | e <- entry ]
        -- idea: print in XML (-x flag), and decode in more detail
        case parseXMLDoc res of
          Nothing -> return Nothing
          Just (Element _ _ xml _) -> return $ parse (onlyElems xml)
  where
        parse :: [Element] -> Maybe Value
        parse [] = Nothing
        parse (Element nm attr cs _:_) = 
                        case showQName nm of
                          "integer" -> Integer <$> parseInteger cs
                          "string"  -> String  <$> parseString cs
                          "dict"    -> Dict    <$> parseDict cs
                          "array"   -> Array   <$> parseArray cs
                          x -> error $ show ("other",x,cs)

        parseInteger :: [Content] -> Maybe Integer
        parseInteger = return . read . concatMap showContent 

        -- "\t" messes up
        parseString :: [Content] -> Maybe Text
        parseString = return . T.pack . concatMap showContent 

        parseDict :: [Content] -> Maybe [(Text,Value)]
        parseDict cs = parseDict' (onlyElems cs)
          where
                  parseDict' :: [Element] -> Maybe [(Text,Value)]
                  parseDict' [] = return []
                  parseDict' (Element nm attr cs _
                             : e
                             : rest) | showQName nm == "key"
                     = do v <- parse [e]
                          ivs <- parseDict' rest
                          return $ (T.pack $ concatMap showContent $ cs, v) : ivs
                  parseDict' _ = Nothing

        parseArray :: [Content] -> Maybe [Value]
        parseArray cs = parseArray' (onlyElems cs)
          where
                  parseArray' :: [Element] -> Maybe [Value]
                  parseArray' [] = return []
                  parseArray' (e : rest)
                     = do v <- parse [e]
                          vs <- parseArray' rest
                          return $ v : vs
                  parseDict' _ = Nothing


-- | Set Entry Value - Sets the value at Entry to Value
-- You can not set dictionaries or arrays.
set :: [Text] -> Value -> PlistBuddy ()
set entry value = PlistBuddy $ do
        Plist pty _ <- ask
        res <- liftIO $ command pty $ "Set "  <> BS.concat [ ":" <> quote e | e <- entry ]
                                      <> " " <> quoteValue value 
        case res of
          "" -> return ()
          _  -> fail $ "set failed: " ++ show res
    
-- | Add Entry Type [Value] - Adds Entry to the plist, with value Value
-- You can add *empty* dictionaries or arrays.
add :: [Text] -> Value -> PlistBuddy ()
add entry value = PlistBuddy $ do
        Plist pty _ <- ask
        suffix <- case value of
                     Array [] -> return ""
                     Array _ -> fail "add: array not empty"
                     Dict [] -> return ""
                     Dict _ -> fail "add: array not empty"
                     _ -> return $ " " <> quoteValue value

        res <- liftIO $ command pty $ "Add "  <> BS.concat [ ":" <> quote e | e <- entry ]
                                      <> " " <> quoteValueType value
                                      <> suffix
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

data Value  = String Text
            | Array [Value]       
            | Dict [(Text,Value)] 
            | Bool Bool
            | Real Double
            | Integer Integer
            | Date ()
            | Data ()
        deriving (Show, Read, Eq, Ord)

quoteValue :: Value -> ByteString
quoteValue (String txt) = quote txt
quoteValue (Array {})   = error "array value"
quoteValue (Dict {})    = error "dict value"
quoteValue (Bool True)  = "true"
quoteValue (Bool False) = "false"
quoteValue (Real r)     = E.encodeUtf8 $ T.pack $ show r
quoteValue (Integer i)  = E.encodeUtf8 $ T.pack $ show i
quoteValue other        = error $ show other ++ " not supported"

quoteValueType :: Value -> ByteString
quoteValueType (String txt) = "string"
quoteValueType (Array {})   = "array"
quoteValueType (Dict {})    = "dict"
quoteValueType (Bool True)  = "bool"
quoteValueType (Bool False) = "bool"
quoteValueType (Real r)     = "real"
quoteValueType (Integer i)  = "integer"
quoteValueType other        = error $ show other ++ " not supported"

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
                    ["-x",fileName]
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


