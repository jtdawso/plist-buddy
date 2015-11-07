{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures, GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Database.PlistBuddy.Path 
        ( Path
        , integer
        , string 
        , (<.>)  
        , (<+>)  
        , fgmap  
        , optional
        , readOnly
        , getPath
        , setPath
        ) where

import Data.Char
import Data.Text(Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

import Database.PlistBuddy


data Path :: * -> * where 
 IntegerPath :: Path Integer
 StringPath :: Path Text
 ElementPath :: Text -> Path a -> Path a
 ZipPath     :: Path a -> Path b -> Path (a,b)
 MapPath   :: (a -> b) -> (b -> a) -> Path a -> Path b
 OptionalPath :: Path a -> Path (Maybe a)
 ReadOnlyPath :: Path a -> Path a

infixr 3 <+>
infixr 9 <.>

integer :: Path Integer
integer = IntegerPath
string  :: Path Text
string = StringPath
(<.>) :: Text -> Path a -> Path a
(<.>) = ElementPath
(<+>)   :: Path a -> Path b -> Path (a,b)
(<+>) = ZipPath
fgmap   :: (a -> b) -> (b -> a) -> Path a -> Path b
fgmap = MapPath 
optional :: Path a -> Path (Maybe a)
optional = OptionalPath
readOnly :: Path a -> Path a
readOnly = ReadOnlyPath

getPath :: Path a -> PlistBuddy a
getPath = getPath' []
 where
   getPath' :: [Text] -> Path a -> PlistBuddy a
   getPath' path IntegerPath = do
           v <- get (reverse path)
           case v of
             (Integer i) -> return i
             _           -> fail $ "bad type of value at " ++ show (path,v)
   getPath' path StringPath = do
           v <- get (reverse path)
           case v of
             (String i) -> return i
             _          -> fail $ "bad type of value at " ++ show (path,v)
   getPath' path (ElementPath e p) = getPath' (e : path) p
   getPath' path (ZipPath p1 p2) = do
           v1 <- getPath' path p1
           v2 <- getPath' path p2
           return $ (v1,v2)
   getPath' path (MapPath f g p1) = do
           v1 <- getPath' path p1
           return $ f v1
   getPath' path (OptionalPath p) =
           (Just <$> getPath' path p) `catchError` \ _ -> return Nothing
   getPath' path (ReadOnlyPath p) = 
           getPath' path p

--setPath :: Path a -> a -> IO ()
setPath :: Path a -> a -> PlistBuddy ()
setPath = setPath' []
 where
   setPath' :: [Text] -> Path a -> a -> PlistBuddy ()
   setPath' path IntegerPath v | not (null path) = do
           set_p (reverse path) (Integer v)
   setPath' path StringPath v | not (null path) = do
           set_p (reverse path) (String v)
   setPath' path (ElementPath e p) v = setPath' (e : path) p v
   setPath' path (ZipPath p1 p2) (v1,v2) = do
           setPath' path p1 v1
           setPath' path p2 v2
   setPath' path (MapPath f g p1) v = do
           setPath' path p1 (g v)
   setPath' path (OptionalPath p) Nothing = return ()
   setPath' path (OptionalPath p) (Just v) = 
           setPath' path p v
   setPath' path (ReadOnlyPath p) _ = return ()

-- Be careful: this assumes the path is dict all the way down
set_p :: [Text] -> Value -> PlistBuddy ()
set_p []   value = fail "can not set_p root"
set_p path value = set path value `catchError` \ _ -> add path value  

test1 = do
        d <- openPlist "test.plist" 
        v <- send d (getPath ("I2" <.> integer))
        print v
        let p1 =  "I1" <.> integer
              <+> "I2" <.> integer
              <+> "S2" <.> string

        v <- send d (getPath p1)
        print v

        send d (setPath p1 (99,(100,"foo")))
        v <- send d (getPath p1)
        print v

        let p2 = "X1" <.> "Y1" <.> "99" <.> "Z1" <.> integer
                                 
        send d (setPath p2 1234)
        v <- send d (getPath p2)
        print v
        send d (get []) >>= print
        
        
        
