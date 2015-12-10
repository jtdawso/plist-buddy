{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Types
        ( -- * Remote Monad
          PlistBuddy(..)
        , throwPlistError
        , catchPlistError
        -- * Remote monad handle
        , Plist(..)
        -- * Other types
        , Value(..)
         -- * Exception
        , PlistBuddyException(..)
        , PlistError(..)
        -- * Audit
        , Audit(..)
        , Trail(..)
        , AuditTrail(..)
        ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except


import Data.Text(Text)
import Data.ByteString (ByteString)
import Data.IORef

import System.Process
import System.Posix.Pty

import Data.Time

import GHC.Generics

------------------------------------------------------------------------------

-- | The Remote Monad
newtype PlistBuddy a = PlistBuddy (ExceptT PlistError (ReaderT Plist IO) a)
  deriving (Functor,Applicative, Monad, MonadError PlistError, MonadReader Plist, MonadIO)

newtype PlistError = PlistError String 
 deriving (Show, Eq)

-- | A version of 'catchError' with the type specialized to 'PlistBuddy'. Using
--   this will cause a static error if used on a non-'PlistBuddy' monad.

catchPlistError :: PlistBuddy a -> (PlistError -> PlistBuddy a) -> PlistBuddy a
catchPlistError = catchError

-- | Throw a 'PlistError'. Uncaught 'PlistError' exceptions will
--   be thrown by 'send' as IO Exceptions.

throwPlistError :: PlistError -> PlistBuddy a
throwPlistError = throwError

-- | The Remote Plist 
data Plist = Plist 
  { plist_pty   :: Pty
  , plist_lock  :: MVar ()  
  , plist_proc  :: ProcessHandle
  , plist_debug :: Bool
  , plist_file  :: FilePath
  , plist_trail :: Trail -> IO ()      -- audit information
  , plist_launder :: IO ()             -- close audit without issuing an exit; for testing
  , plist_dirty  :: IORef (Maybe Bool) -- if the database has been changed; Nothing == closed
  }
        
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

------------------------------------------------------------------------------

-- | 'PlistBuddyException' is for fatal things,
--  like the sub-process blocks, for some reason.
data PlistBuddyException = PlistBuddyException String
    deriving (Show, Generic)

instance Exception PlistBuddyException

------------------------------------------------------------------------------

data AuditTrail = AuditTrail ByteString [Trail] (Maybe ByteString)
   deriving (Show,Read,Generic)  
   
data Audit
 = UTCTime :! Trail 
   deriving (Show,Read,Generic)  
   
data Trail 
  = Save ByteString       -- ^ hash code of saved file
  | Revert
  | Exit
  | Clear Value
  | Set [Text] Value
  | Add [Text] Value
  | Delete [Text]
  | Start ByteString      -- ^ hash code at start of audit capture
    deriving (Show,Read,Generic)  

