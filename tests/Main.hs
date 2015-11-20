{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
import Database.PlistBuddy 

import Data.Monoid
import Data.Text(Text,pack)
import Data.Text.IO as TIO
import Data.Time
import qualified Data.ByteString as BS

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (evaluate, bracket)

import qualified System.IO as IO
import System.Timeout
import Control.Concurrent (threadDelay)
import System.Mem

import Data.List (sortBy)

clearDB :: IO ()
clearDB = do
  TIO.writeFile "test.plist" "{}" 
  performMajorGC

openConnection :: IO Plist
openConnection = 
--    debugOn <$> 
    openPlist "test.plist"
        
closeConnection :: Plist -> IO ()
closeConnection d = do
    send d $ exit

-- only for tests that write then read
withPlistConnection :: (Plist -> IO ()) -> IO ()
withPlistConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ beforeAll clearDB $ do
  describe "inital plist" $  modifyMaxSuccess (\ x -> 100) $ do

    it "check initial dict is an dictionary" $ withPlistConnection $ \ d -> do
      r0 <- send d $ get []
      r0 `shouldBe` Dict []

    it "check reset to array" $ withPlistConnection $ \ d -> do
      _ <- send d $ clear (Array [])
      r0 <- send d $ get []
      r0 `shouldBe` Array []
      
    it "check reset to back to an dict" $ withPlistConnection $ \ d -> do
      _ <- send d $ clear (Array [])
      _ <- send d $ clear (Dict [])
      r0 <- send d $ get []
      r0 `shouldBe` Dict []

    it "check adding a value at top level" $ 
      property $ \ (v :: Value) -> withPlistConnection $ \ d -> do
        _ <- send d $ add ["I1"] v
        r0 <- send d $ get []
        r0 `shouldBe` Dict [("I1",v)]


main2 = do
        IO.hSetBuffering IO.stdout IO.NoBuffering
        IO.hSetBuffering IO.stderr IO.NoBuffering
        IO.hSetBuffering IO.stdin  IO.NoBuffering
        TIO.writeFile "test.plist" "{}"

        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "initial dict" r0 $ Dict []

        _ <- send d $ clear (Array [])
        r1 <- send d $ get []
        check "reset as an array" r1 $ Array []

        _ <- send d $ clear (Dict [])
        r2 <- send d $ get []
        check "reset as a dict" r2 $ Dict []

        _ <- send d $ clear (Array [])
        r3 <- send d $ get []
        check "reset as an array (2)" r3 $ Array []
        send d $ exit

        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "initial dict still there" r0 $ Dict []    

        _ <- send d $ clear (Array [])
        r5 <- send d $ get []
        check "reset as an array (3)" r5 $ Array []

        send d $ save
        send d $ exit
        
        d <- openPlist "test.plist"        
        
        r6 <- send d $ get []
        check "array after save" r6 $ Array []
        
        _ <- send d $ clear (Dict [])
        r7 <- send d $ get []
        check "change to dict" r7 $ Dict []

        send d $ revert -- should revert to the array

        r8 <- send d $ get []
        check "array after save" r8 $ Array []
        
        _ <- send d $ clear (Dict []) 
        r9 <- send d $ get []
        check "change to dict" r9 $ Dict []

        _ <- send d $ add ["S1"] (String "")
        _ <- send d $ add ["S2"] (String "Hello")
        _ <- send d $ add ["I1"] (Integer 0)
        _ <- send d $ add ["I2"] (Integer 123)
        r10 <- send d $ get []
        check "dict with 4 entries" r10 $ Dict 
                [("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]

        _ <- send d $ add ["D1"] (Dict [])
        _ <- send d $ add ["A1"] (Array [])
        r10 <- send d $ get []
        check "dict with 6 entries" r10 $ Dict 
                [("A1",Array[])
                ,("D1",Dict[])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]

        _ <- send d $ add ["D1","X"] (Integer 99)
        _ <- send d $ add ["A1",""] (Integer 1) -- add at end
        _ <- send d $ add ["A1",""] (Integer 2)  -- add at end
        _ <- send d $ add ["A1","2"] (Integer 3) -- add at end
        _ <- send d $ add ["A1","0"] (Integer 4) -- add at start
        r10 <- send d $ get []

        check "dict with 11 entries" r10 $  Dict 
                [("A1",Array 
                        [Integer 4
                        ,Integer 1
                        ,Integer 2
                        ,Integer 3])
                ,("D1",Dict
                        [("X",Integer 99)
                        ])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]

        _ <- send d $ delete ["A1","1"] 
        r10 <- send d $ get []
        check "dict with 10 entries" r10 $ Dict
                [("A1",Array 
                        [Integer 4
                        ,Integer 2
                        ,Integer 3])
                ,("D1",Dict
                        [("X",Integer 99)
                        ])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]

        a1 <- send d $ get ["A1"]
        check "get array" a1 $  Array 
                        [Integer 4
                        ,Integer 2
                        ,Integer 3]

        a2 <- send d $ get ["A1","1"]
        check "get array value" a2 $                
                Integer 2

        d1 <- send d $ get ["D1"]
        check "get dict" d1 $ Dict              
                [("X",Integer 99)
                ]

        d2 <- send d $ get ["D1","X"]
        check "get dict value" d2 $                
                Integer 99

        _ <- send d $ set ["D1","X"] (Integer 3432)
        d3 <- send d $ get ["D1","X"]
        check "get dict value (2)" d3 $                
                Integer 3432

        _ <- send d $ delete ["D1"]
        r10 <- send d $ get []
        check "dict with dict removed" r10 $ Dict
                [("A1",Array 
                        [Integer 4
                        ,Integer 2
                        ,Integer 3])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]

        _ <- send d $ set ["A1","2"] (Integer 1234)
        r10 <- send d $ get []
        check "testing set" r10 $ Dict
                [("A1",Array 
                        [Integer 4
                        ,Integer 2
                        ,Integer 1234])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]
        _ <- send d $ save
        _ <- send d $ exit
        return ()

        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "updated dict still there" r0 $   Dict
                [("A1",Array 
                        [Integer 4
                        ,Integer 2
                        ,Integer 1234])
                ,("I1",Integer 0)
                ,("I2",Integer 123)
                ,("S1",String "")
                ,("S2",String "Hello")
                ]
        
        -- try get type error
        
        r <- send d $ ((set ["I1"] (String "foo")>>return "no failed") `catchError` \ msg -> return msg)
        check "check for type error" r $ "set failed: \"Unrecognized Integer Format\""

        _ <- send d $ exit

        d <- openPlist "test.plist"

        now <- getCurrentTime

	send d $ add ["S5"] (Date now)

        Date r0 <- send d $ get ["S5"]
        
        check "check for date storage" (abs (diffUTCTime now r0) < 1) $ True
        _ <- send d $ exit

        d <- debugOn <$> openPlist "test.plist"

{-
	send d $ add ["S6"] (Data $ BS.pack $ [10..20])

        Data r0 <- send d $ get ["S6"]
        
        print r0
        _ <- send d $ exit
-}




        return ()


check :: (Eq a, Show a) => Text -> a -> a -> IO ()
check msg t1 t2 = if t1 /= t2 then fail ("check failed: " ++ show (msg,t1,t2)) else TIO.putStrLn msg


instance Arbitrary Value where
    arbitrary = sized $ \ n -> arbitraryValue n
    
arbitraryValue :: Int -> Gen Value
arbitraryValue 0 = oneof 
    [ Integer <$> arbitrary
    , String  <$> arbitraryText
    , Bool    <$> arbitrary
    , Real    <$> arbitrary
--    , Date    <$> arbitraryDate
--    , Data    <$> arbitraryData -- not supported yet
    ]
arbitraryValue n = arbitraryValue 0 

arbitraryDate :: Gen UTCTime
arbitraryDate =
  UTCTime <$> ((\ d -> addDays d (fromGregorian 1970 1 1))
                  <$> choose (0,100 * 365)
              )
          <*> (fromInteger <$> choose (0,60 * 60 * 24 - 1))
        
arbitraryText :: Gen Text
arbitraryText = sized $ \ n -> pack <$> (vectorOf (n`div`10) $ elements ('\n':[' '..'~']))

arbitraryData :: Gen BS.ByteString
arbitraryData = sized $ \ n -> BS.pack <$> (vectorOf (n`div`10) $ elements ([32..126]))

instance Eq Value where
  (==) = eqValue

eqValue :: Value -> Value -> Bool
eqValue (String s1) (String s2) = s1 == s2
eqValue (Array a1)  (Array a2)  = a1 == a2
eqValue (Dict d1)   (Dict d2)   = sortBy f d1 == sortBy f d2  -- order should not matter
  where f (a,_) (b,_) = a `compare` b
eqValue (Bool a1)   (Bool a2)   = a1 == a2
eqValue (Real a1)   (Real a2)   = abs (a1 - a2) < 1e-3
eqValue (Integer a1) (Integer a2) = a1 == a2
eqValue (Date d1)   (Date d2)      = d1 == d2
eqValue (Data d1)   (Data d2)      = d1 == d2
eqValue _ _ = False
