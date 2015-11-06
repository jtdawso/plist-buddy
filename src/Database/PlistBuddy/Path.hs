{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Path where

import Data.Text(Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Database.PlistBuddy

data Path :: * -> * where 
 IntegerPath :: Path Integer
 ElementPath :: Text -> Path a -> Path a
 IndexPath   :: Int  -> Path a -> Path a
 ZipPath     :: Path a -> Path b -> Path (a,b)

getPath :: Path a -> PlistBuddy a
getPath = getPath' []
 where
   getPath' :: [Text] -> Path a -> PlistBuddy a
   getPath' path IntegerPath = do
           v <- get (reverse path)
           case v of
             Just (Integer i) -> return i
             _                -> fail $ "bad type of value at " ++ show (path,v)
   getPath' path (ElementPath e p) = getPath' (e : path) p
   getPath' path (ZipPath p1 p2) = do
           v1 <- getPath' path p1
           v2 <- getPath' path p2
           return $ (v1,v2)

--setPath :: Path a -> a -> IO ()
--

test1 = do
        d <- openPlist "test.plist" 
        v <- send d (getPath (ElementPath "I2" $ IntegerPath ))
        print v
        v <- send d (getPath (ZipPath (ElementPath "I1" $ IntegerPath)
                                      (ElementPath "I2" $ IntegerPath)))
        print v
                                 