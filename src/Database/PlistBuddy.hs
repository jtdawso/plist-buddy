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
        -- * Debugging
        , debugOn
         -- * Exception
        , PlistBuddyException(..)
         -- * Audit
        , Trail(..)
        , AuditTrail(..)
        , auditOn
        , auditOff
        , replay
        , recover
        , hashcode
        , findTrail
         -- * Background version of Plist
        , BackgroundPlist
        , backgroundPlist
        , bgSend
        ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except

import Data.Char (ord,isSpace,isDigit)
import Data.IORef
import Data.Text(Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Database.PlistBuddy.Audit
import Database.PlistBuddy.Command
import Database.PlistBuddy.Open
import Database.PlistBuddy.Types as Types

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

debugOn :: Plist -> Plist
debugOn p = p { plist_debug = True }

send :: Plist -> PlistBuddy a -> IO a
send dev (PlistBuddy m) = bracket (takeMVar lock) (putMVar lock) $ \ () -> do
        d <- readIORef (plist_dirty dev)
        case d of
          Just {} -> do
            v <- runReaderT (runExceptT m) dev
            case v of
              Left (PlistError msg) -> fail msg  -- an unhandled PlistError turns into an IO fail
              Right val -> return val
          Nothing -> throw $ PlistBuddyException $ "plist handle has been closed with exit"
  where lock = plist_lock dev

-- | Returns Help Text
help :: PlistBuddy Text
help = do
        plist <- ask
        res <- liftIO $ command plist "Help"
        return $ E.decodeUtf8 $ res

-- | Exits the program, changes are not saved to the file
exit :: PlistBuddy ()
exit = do
        plist <- ask
        liftIO $ plist_trail plist Exit
        liftIO $ do
            (void $ command plist "Exit") `catch` \ (e :: IOException) -> do { return () }
        debug ("waiting for Process on exit")
        r <- liftIO $ do
            waitForProcess (plist_proc plist)
        debug ("closing pty after process closed",r)
        liftIO $ do
            closePty (plist_pty plist)
        debug ("done with exit, including closing pty")
        liftIO $ writeIORef (plist_dirty plist) $ Nothing -- closed
        return ()

-- | Saves the current changes to the file
save :: PlistBuddy ()
save = do
        plist <- ask
        res <- liftIO $ command plist "Save"
        case res of
          "Saving..." -> do
                bs <- liftIO $ hashcode (plist_file plist)
                liftIO $ plist_trail plist $ Save bs
                dirty False
                return ()
          _ -> error $ "save failed: " <> show res


-- | Reloads the last saved version of the file
revert :: PlistBuddy ()
revert = do
        plist <- ask
        res <- liftIO $ command plist "Revert"
        case res of
          "Reverting to last saved state..." -> do
            liftIO $ plist_trail plist Revert
            dirty False
            return ()
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
          "Initializing Plist..." -> do
            liftIO $ plist_trail plist $ Clear value
            dirty True
            return ()
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
set entry (Date d) = mergeDate entry d (Set entry $ Date $ d)
set entry (Data d) = importData entry d (Set entry $ Data $ d)
set entry (Dict xs) = error "set: dict not allowed"
set entry (Array xs) = error "set: array not allowed"
set entry value = do
        debug ("set",entry,value,valueType value)
        plist <- ask
        dirty True
        res <- liftIO $ command plist $ "Set " <> BS.concat [ ":" <> quoteText e | e <- entry ]
                                      <> " " <> quoteValue value
        case res of
          "" -> do 
            liftIO $ plist_trail plist $ Set entry value
            return ()
          "Unrecognized Date Format" -> error $ "Unrecognized"
          _  -> throwPlistError $ PlistError $ "set failed: " ++ show res
    
-- | Add Entry Type [Value] - Adds Entry to the plist, with value Value
-- You can add *empty* dictionaries or arrays.
add :: [Text] -> Value -> PlistBuddy ()
add [] value = error "Can not add to an empty path"
add entry (Date d) = mergeDate entry d (Add entry $ Date $ d)
add entry (Data d) = importData entry d (Add entry $ Data $ d)
add entry (Dict xs) | not (null xs) = error "add: dict not empty"
add entry (Array xs) | not (null xs) = error "add: array not empty"
add entry value = do
        debug ("add",entry,value,valueType value)
        plist <- ask
        dirty True
        res <- liftIO $ command plist $ "Add "  <> BS.concat [ ":" <> quoteText e | e <- entry ]
                                      <> " " <> valueType value <> " "
                                      <> quoteValue value
        case res of
          "" -> do
            liftIO $ plist_trail plist $ Add entry value
            return ()
          _  -> throwPlistError $ PlistError $ "add failed: " ++ show res

-- | Delete Entry - Deletes Entry from the plist
delete :: [Text] -> PlistBuddy ()
delete entry = do
        debug ("delete",entry)
        plist <- ask
        dirty True
        res <- liftIO $ command plist $ "delete " <>  BS.concat [ ":" <> quoteText e | e <- entry ]
        case res of
          "" -> do
            liftIO $ plist_trail plist $ Delete entry
            return ()
          _  -> throwPlistError $ PlistError $ "delete failed: " ++ show res


importData :: [Text] -> ByteString -> Trail -> PlistBuddy ()
importData entry d t = do
  debug ("import(add/set)",entry,d)
  plist <- ask
  dirty True
  nm <- liftIO $ do
    (nm,h) <- openBinaryTempFile "/tmp" "plist-data-.tmp"
    BS.hPutStr h d -- write temp file with the binary data
    hClose h
    return nm
  res <- liftIO $ command plist $ "Import "  <> BS.concat [ ":" <> quoteText e | e <- entry ]
                                <> " "
                                <> (quoteText $ T.pack $ nm)
  liftIO $ removeFile nm
  case res of
    "" -> do
      liftIO $ plist_trail plist t
      return ()
    _  -> throwPlistError $ PlistError $ "import(add/set) failed: " ++ show res


-- a version of add/set that uses merge, because the date writing format
-- has a missing hour in Fall, because of timezones.
mergeDate :: [Text] -> UTCTime -> Trail -> PlistBuddy ()
mergeDate entry d t = do
  debug ("merge(set/get)",entry,d)
  plist <- ask
  dirty True
  v <- get (init entry) -- 'orable way of doing this. Best of bad options
  case v of
    Dict env -> do
      res <- liftIO $ do
            (nm,h) <- openBinaryTempFile "/tmp" "plist-date-.tmp"
            hPutStr h $ showTopElement $ 
                          unode "dict" $
                              [ unode "key"  $ T.unpack $ last entry
                              , unode "date" $ formatTime defaultTimeLocale "%FT%XZ" d
                              ]
            hClose h
            when (last entry `elem` map fst env) $ do
              void $ command plist $ "delete " <>  BS.concat [ ":" <> quoteText e | e <- entry ]
            res <- command plist $ "merge " <> quoteText (T.pack nm) <> " "
                                            <> BS.concat [ ":" <> quoteText e | e <- (init entry) ]
            removeFile nm
            return res
      case res of
        "" -> do
          liftIO $ plist_trail plist t
          return ()
        _  -> throwPlistError $ PlistError $ "merge(set/get) failed: " ++ show res
    Array vs | T.all isDigit (last entry) -> do
      res <- liftIO $ do
            (nm,h) <- openBinaryTempFile "/tmp" "plist-date-.tmp"
            hPutStr h $ showTopElement $ 
                          unode "array" $
                              [ unode "date" $ formatTime defaultTimeLocale "%FT%XZ" d
                              ]
            hClose h
            -- add to end of list
            res <- command plist $ "merge " <> quoteText (T.pack nm) <> " "
                                            <> BS.concat [ ":" <> quoteText e | e <- (init entry) ]
            removeFile nm
            -- now, move the inserted value to the correct place
            let n = if T.null (last entry)
                    then length vs  -- "" inserts at the end
                    else read $ T.unpack $ last $ entry

            when (n < length vs) $ do
              -- We need to move it
             let path x = BS.concat [ ":" <> quoteText e 
                                    | e <- init entry ++ [T.pack $ show $ x]
                                    ] 
             void $ command plist $ "copy " <> path (length vs) <> " " <> path n
             void $ command plist $ "delete " <> path (length vs)
            return res
      case res of
        "" -> return ()
        _  -> throwPlistError $ PlistError $ "merge(set/get) failed: " ++ show res

    _ -> error $ "add/set error for date; path type error"
  

dirty :: Bool -> PlistBuddy ()
dirty b = do
  plist <- ask
  liftIO $ writeIORef (plist_dirty plist) $ Just b


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
        esc c  = BS.pack [c]


quoteValue :: Value -> ByteString
quoteValue (String txt) = quoteBS $ E.encodeUtf8 $ txt
quoteValue (Array {})   = ""
quoteValue (Dict {})    = ""
quoteValue (Bool True)  = "true"
quoteValue (Bool False) = "false"
quoteValue (Real r)     = E.encodeUtf8 $ T.pack $ show r
quoteValue (Integer i)  = E.encodeUtf8 $ T.pack $ show i
quoteValue other        = error $ "can not quote " ++ show other

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

debug :: (Show a) => a -> PlistBuddy ()
debug a = do
        plist <- ask
        when (plist_debug plist) $ do
                liftIO $ do
                  tid <- myThreadId
                  print (tid,a)


------------------------------------------------------------------------------


-- | 'replay' invokes the respective 'PlistBuddy' function. It is uses
--  when replying an audit replay.
replay :: Trail -> PlistBuddy ()
replay (Save {})  = save
replay Revert     = revert
replay Exit       = exit
replay (Clear v)  = clear v
replay (Set p v)  = set p v
replay (Add p v)  = add p v
replay (Delete p) = delete p
replay (Types.Start {}) = return ()


--------------------------------------

-- | This is a version of Plist that saves the database on regular occasions,
--   and suspends itself when not used.
data BackgroundPlist = BackgroundPlist Int (IO Plist) (MVar BackgroundState)

data BackgroundState
  = Sleeping
  | Awake Plist

-- | This creates a background Plist. The 'IO Plist' may be called many times.  
backgroundPlist :: Int -> IO Plist -> IO BackgroundPlist
backgroundPlist n p = do
  v <- newMVar Sleeping
  return $ BackgroundPlist n p v

-- | Send a command to a background Plist.
--   The semantics of bgSend is the same as saving after every command, provided you wait long enough
bgSend :: BackgroundPlist -> PlistBuddy a -> IO a
bgSend bg@(BackgroundPlist n p v) m = do
  st <- takeMVar v
  case st of
    Sleeping -> do
        dev <- p
        forkIO $ autoSave bg
        r <- send dev m -- TODO: handle exceptions here
        putMVar v $ Awake dev  
        return r
    Awake dev -> do
        r <- send dev m -- TODO: handle exceptions here
        putMVar v $ Awake dev  
        return r

autoSave :: BackgroundPlist -> IO ()
autoSave bg@(BackgroundPlist n p v) = do
  threadDelay (n * 1000 * 1000)
  st <- takeMVar v
  case st of
    Sleeping -> putMVar v Sleeping
    Awake dev -> do
        -- assumes no one else is read/writing. The MVar BackgroundState is acting as a lock
        d <- readIORef (plist_dirty dev)
        (case d of
          Nothing    -> return ()
          Just True  -> send dev $ do { save ; exit } 
          Just False -> send dev $ do { exit }) `finally` putMVar v Sleeping          
        

