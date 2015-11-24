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
        , Transaction(..)
        , Write(..)
        ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Except

import Data.Text(Text)
import Data.ByteString (ByteString)

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

data Transaction 
 = Transaction [Write]
 | Hash ByteString
   deriving (Show,Read,Generic)  
   
data Write 
  = Save
  | Revert
  | Exit
  | Clear Value
  | Set [Text] Value
  | Add [Text] Value
  | Delete [Text]
   deriving (Show,Read,Generic)  

