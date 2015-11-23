{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy 
        ( -- * Remote Monad
          PlistBuddy()
        , openPlist
        , Plist()
        , send
        , throwPlistError
        , catchPlistError
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
        , Value(..)
        , valueType
        , debugOn
         -- * Exception
        , PlistBuddyException(..)
        ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except

import Data.Char (ord,isSpace)
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.List ()
import Data.Monoid ((<>))

import System.Directory (removeFile)
import System.Process
import System.IO
import System.Posix.Pty
import System.Timeout

import Text.XML.Light as X

import Data.Time
import Data.Either(either)

import GHC.Generics
import Debug.Trace
import System.IO.Error (catchIOError)

------------------------------------------------------------------------------

-- | The Remote Monad
newtype PlistBuddy a = PlistBuddy (ExceptT PlistError (ReaderT Plist IO) a)
  deriving (Functor,Applicative, Monad, MonadError PlistError, MonadReader Plist, MonadIO)

data PlistError = PlistError String 
 deriving (Show, Eq)

-- | A version of `catchError` with the type specialized to PlistBuddy. Using
--   this will cause a static error if used on a non-PlistBuddy monad.

catchPlistError :: PlistBuddy a -> (PlistError -> PlistBuddy a) -> PlistBuddy a
catchPlistError = catchError

-- | Throw a `PlistError`. Uncaught `PlistError` exceptions will
--   be thrown by `send` as IO Exceptions.

throwPlistError :: PlistError -> PlistBuddy a
throwPlistError = throwError

-- | The Remote Plist 
data Plist = Plist Pty (MVar ()) ProcessHandle Bool

debugOn :: Plist -> Plist
debugOn (Plist pty lock h _) = Plist pty lock h True

send :: Plist -> PlistBuddy a -> IO a
send dev@(Plist pty lock _ _) (PlistBuddy m) = bracket (takeMVar lock)  (putMVar lock) $ \ () -> do
        v <- runReaderT (runExceptT m) dev
        case v of
          Left (PlistError msg) -> fail  msg  -- an unhandled PlistError turns into an IO fail
          Right val -> return val

-- | Returns Help Text
help :: PlistBuddy Text
help = do
        plist <- ask
        res <- liftIO $ command plist "Help"
        return $ E.decodeUtf8 $ res

-- | Exits the program, changes are not saved to the file
exit :: PlistBuddy ()
exit = do
        plist@(Plist pty _ ph _) <- ask
        liftIO $ do
            (void $ command plist "Exit") `catch` \ (e :: IOException) -> do { return () }
        debug ("waiting for Process on exit")
        r <- liftIO $ do
            waitForProcess ph
        debug ("closing pty after process closed",r)
        liftIO $ do
            closePty pty
        debug ("done with exit, including closing pty")
        return ()

-- | Saves the current changes to the file
save :: PlistBuddy ()
save = do
        plist <- ask
        res <- liftIO $ command plist "Save"
        case res of
          "Saving..." -> return ()
          _ -> error $ "save failed: " <> show res


-- | Reloads the last saved version of the file
revert :: PlistBuddy ()
revert = do
        plist <- ask
        res <- liftIO $ command plist "Revert"
        case res of
          "Reverting to last saved state..." -> return ()
          _ -> error $ "revert failed: " ++ show res

-- | Clear Type - Clears out all existing entries, and creates root of a value,
-- where the value is an empty Dict or Array.
clear :: Value -> PlistBuddy ()
clear value = do
        plist <- ask
        ty <- case value of
                     Array [] -> return $ valueType value
                     Array _  -> error "add: array not empty"
                     Dict []  -> return $ valueType value
                     Dict _   -> error "add: dict not empty"
                     _        -> error "adding a non dict/array to the root path"
        res <- liftIO $ command plist $ "Clear " <> ty
        case res of
          "Initializing Plist..." -> return ()
          _  -> fail $ "add failed: " ++ show res

-- | Print Entry - Gets value of Entry.
get :: [Text] -> PlistBuddy Value
get entry = do
        debug ("get",entry)
        plist <- ask
        res <- liftIO $ command plist $ "Print" <>  BS.concat [ ":" <> quoteText e | e <- entry ]
        if "Print: Entry, " `BS.isPrefixOf` res && ", Does Not Exist" `BS.isSuffixOf` res
        then throwPlistError $ PlistError $ "value not found"
        else case parseXMLDoc (BS.filter (/= fromIntegral (ord '\r')) res) of
          Nothing -> error "get: early parse error"
          Just (Element _ _ xml _) -> case parse (onlyElems xml) of
                                        Nothing -> error ("get: late parse error : " ++ show (onlyElems xml))
                                        Just v -> return $  v
  where
        parse :: [Element] -> Maybe Value
        parse [] = Nothing
        parse (Element nm attr cs _:_) = 
                        case showQName nm of
                          "integer" -> Integer <$> parseInteger cs
                          "string"  -> String  <$> parseString cs
                          "dict"    -> Dict    <$> parseDict cs
                          "array"   -> Array   <$> parseArray cs
                          "false"   -> return $ Bool False
                          "true"    -> return $ Bool True
                          "real"    -> Real    <$> parseReal cs
                          "data"    -> Data    <$> parseData cs
                          "date"    -> Date    <$> parseDate cs
                          x -> error $ show ("other",x,cs)

        parseInteger :: [Content] -> Maybe Integer
        parseInteger = return . read . concatMap showContent 

        parseReal :: [Content] -> Maybe Double
        parseReal = return . read . concatMap showContent 

        -- The content must be encoded as an ISO-8601 string in the UTC timezone
        -- https://code.google.com/p/networkpx/wiki/PlistSpec#date
        parseDate :: [Content] -> Maybe UTCTime
        parseDate = parseTimeM True defaultTimeLocale "%FT%XZ"
                  . concatMap showContent 

        parseData :: [Content] -> Maybe ByteString
        parseData = either (const Nothing)
                           (Just) 
                  . B64.decode
                  . E.encodeUtf8
                  . T.filter (not . isSpace)
                  . T.pack
                  . showContents

        -- "\t" messes up
        parseString :: [Content] -> Maybe Text
        parseString = return . T.pack . showContents 

        showContents :: [Content] -> String
        showContents = concatMap showContent
          where        
            showContent :: Content -> String
            showContent (Elem e) = error "internal Elem"
            showContent (Text e) = case cdVerbatim e of
              CDataText     -> cdData e
              CDataVerbatim -> error "internal CDataVerbatim"
              CDataRaw      -> error "internal CDataRaw"
            showContent (CRef e) = error "internal CRef"

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
set []    value = error "Can not set empty path"
set entry value = do
        qv <- liftIO $ quoteValue value
        debug ("set",entry,value,qv,valueType value)
        plist <- ask
        let cmd = case qv of
                    RawQuote {} -> "Set "
                    FileQuote {} -> "Import "
        res <- liftIO $ command plist $ cmd <> BS.concat [ ":" <> quoteText e | e <- entry ]
                                      <> " " <> showQuote qv
        liftIO $ finalizeQuote qv
        case res of
          "" -> return ()
          "Unrecognized Date Format" -> error $ "Unrecognized"
          _  -> throwPlistError $ PlistError $ "set failed: " ++ show res
    
-- | Add Entry Type [Value] - Adds Entry to the plist, with value Value
-- You can add *empty* dictionaries or arrays.
add :: [Text] -> Value -> PlistBuddy ()
add [] value = error "Can not add to an empty path"
add entry value = do
        qv <- liftIO $ quoteValue value
        debug ("add",entry,value,qv,valueType value)
        plist <- ask
        suffix <- case value of
                     Array [] -> return ""
                     Array _ -> error "add: array not empty"
                     Dict [] -> return ""
                     Dict _ -> error "add: array not empty"
                     _ -> do return $ showQuote qv
        let cmd = case qv of
                    RawQuote {} -> "Add "
                    FileQuote {} -> "Import "
        let ty = case qv of
                    RawQuote {} -> valueType value
                    FileQuote {} -> ""

        res <- liftIO $ command plist $ cmd  <> BS.concat [ ":" <> quoteText e | e <- entry ]
                                      <> " " <> ty <> " "
                                      <> suffix
        liftIO $ finalizeQuote qv
        case res of
          "" -> return ()
          _  -> throwPlistError $ PlistError $ "add failed: " ++ show res

-- | Delete Entry - Deletes Entry from the plist
delete :: [Text] -> PlistBuddy ()
delete entry = do
        debug ("delete",entry)
        plist <- ask
        res <- liftIO $ command plist $ "delete " <>  BS.concat [ ":" <> quoteText e | e <- entry ]
        case res of
          "" -> return ()
          _  -> throwPlistError $ PlistError $ "delete failed: " ++ show res

{-                
-- Not (yet) supported
--    Copy <EntrySrc> <EntryDst> - Copies the EntrySrc property to EntryDst
--    Merge <file.plist> [<Entry>] - Adds the contents of file.plist to Entry
--    Import <Entry> <file> - Creates or sets Entry the contents of file
-}

quoteText :: Text -> ByteString
quoteText = quoteBS . E.encodeUtf8

quoteBS :: ByteString -> ByteString
quoteBS q = "'" <> BS.concatMap esc q <> "'"
  where esc 39 = "\\'"
        esc 92 = "\\\\"  -- RTT moment
        esc 10 = "\\n"
        esc 34 = "\\\""
--        esc 28 = "\\u001C" 
--        esc 26 = "\\z"
        esc c  = BS.pack [c]

        
------------------------------------------------------------------------------

data Value  = String Text
            | Array [Value]       
            | Dict [(Text,Value)] 
            | Bool Bool
            | Real Double
            | Integer Integer
            | Date UTCTime
            | Data ByteString
        deriving (Show, Read, Generic)


data Quote = RawQuote ByteString          -- raw text
           | FileQuote ByteString (IO ()) -- name of file, and finalizer

instance Show Quote where
  show (RawQuote bs)    = "RawQuote:" ++ show bs
  show (FileQuote bs _) = "FileQuote:" ++ show bs

showQuote :: Quote -> ByteString
showQuote (RawQuote bs)    = bs
showQuote (FileQuote bs _) = bs

finalizeQuote :: Quote -> IO ()
finalizeQuote (RawQuote {}) = return ()
finalizeQuote (FileQuote _ m) = m

quoteValue :: Value -> IO Quote
quoteValue (String txt) = return $ RawQuote $ quoteBS $ E.encodeUtf8 $ txt
quoteValue (Array {})   = return $ RawQuote $ ""
quoteValue (Dict {})    = return $ RawQuote $ ""
quoteValue (Bool True)  = return $ RawQuote $ "true"
quoteValue (Bool False) = return $ RawQuote $ "false"
quoteValue (Real r)     = return $ RawQuote $ E.encodeUtf8 $ T.pack $ show r
quoteValue (Integer i)  = return $ RawQuote $ E.encodeUtf8 $ T.pack $ show i
--  for some reason, PlistBuddy does not access UTC, but needs an actual zone.
quoteValue (Date d)     = do
           -- PlistBuddy does not accept UTC as a zone for writing,
           -- but uses it as the output when reading.
           -- So we need to convert to the local zone, aka for strptime(3).
--one <- getTimeZone t
--	return (utcToZonedTime zone t)
{-  
          tz <- getTimeZone $ d -- addUTCTime 100000 d -- (-60 * 60) d
          tz1 <- getTimeZone $ addUTCTime (-24 * 60 * 60) d -- (-60 * 60) d
          
          let special = (tz1 /= tz) &&
                        utctDayTime d >= 7 * 60 * 60 
                      &&
                        utctDayTime d < 8 * 60 * 60
          
                                  if (tz /= tz1) 
          then print (tz,tz1, utctDayTime d)
          else return ()
-}
          return $ RawQuote 
                 $ E.encodeUtf8 
                 $ T.pack 
                 $ formatTime defaultTimeLocale "%a %b %e %H:%M:%S GMT %Y" 
--                 $ utcToZonedTime (if special then tz1 else tz) 
                 $ d
quoteValue (Data d) | BS.null d  = return $ RawQuote $ ""
quoteValue (Data d) = do
  (nm,h) <- openBinaryTempFile "/tmp" "plist-data-.tmp"
  BS.hPutStr h d -- write temp file with the binary data
  hClose h
  return $ FileQuote (quoteText $ T.pack $ nm) (removeFile nm)

-- Mon Oct 27 20:06:30 CST 2014

valueType :: Value -> ByteString
valueType (String txt) = "string"
valueType (Array {})   = "array"
valueType (Dict {})    = "dict"
valueType (Bool True)  = "bool"
valueType (Bool False) = "bool"
valueType (Real r)     = "real"
valueType (Integer i)  = "integer"
valueType (Date {})    = "date"
valueType (Data {})    = "data"

------------------------------------------------------------------------------

handleIOErrors :: IO a -> IO a
handleIOErrors m =
  m `catchIOError` \ e -> throw $ PlistBuddyException $ "IO error, " ++ show e

openPlist :: FilePath -> IO Plist
openPlist fileName = handleIOErrors $ do
    tid <- myThreadId 
    (pty,ph) <- spawnWithPty
                    Nothing
                    False
                    "/usr/libexec/PlistBuddy"
                    ["-x",fileName]
                    (80,24)

    myWritePty pty "#\n" -- 1 times in 100, you need to poke the plist-buddy
    _ <- recvReply0 pty True
    attr <- getTerminalAttributes pty
    setTerminalAttributes pty ((attr `withoutMode` EnableEcho) `withoutMode` ProcessInput) Immediately
    lock <- newMVar ()
    return $ Plist pty lock ph False

command :: Plist -> ByteString -> IO ByteString
command (Plist pty lock _ d) input = todo
  where
    write txt = do
      when d $ print ("write",txt)
      myWritePty pty txt

    debug msg = when d $ do
      tid <- myThreadId
      print (tid,msg)

    todo = do
        write (input <> "\n")
        r <- recvReply pty d
        when d $ print ("read",r)
        return $ r


recvReply0 :: Pty -> Bool -> IO ByteString
recvReply0 pty d = readMe []
  where
    prompt = "\nCommand: "

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

recvReply :: Pty -> Bool -> IO ByteString
recvReply pty d = readMe []
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

-----------------------------

-- | 'PlistBuddyException' is for fatal things,
--  like the sub-process blocks, for some reason.
data PlistBuddyException = PlistBuddyException String
    deriving (Show, Generic)

instance Exception PlistBuddyException

-----------------------------


debug :: (Show a) => a -> PlistBuddy ()
debug a = do
        Plist _ _ _ d <- ask
        when d $ do
                liftIO $ do
                  tid <- myThreadId
                  print (tid,a)
                
                