{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
import Database.PlistBuddy 

import Data.Monoid
import Control.Monad (when)
import Data.Text(Text,pack,unpack)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Time
import qualified Data.ByteString as BS

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Exception
import Control.Exception (evaluate, bracket, catch)

import qualified System.IO as IO
import System.Timeout
import Control.Concurrent (threadDelay)
import System.Mem

import Data.List (sortBy, sort, nub, transpose,lookup)

import GHC.Generics
import Control.Monad.Reader

import System.Environment
import Data.Char (isDigit)

clearDB :: IO ()
clearDB = do
  TIO.writeFile "test.plist" "{}" 
  TIO.writeFile "test.audit" ""
  

openConnection :: Bool -> IO Plist
openConnection audit = do
    d <- openPlist "test.plist"
    send d $ clear (Dict [])
    if audit 
    then auditOn "test.audit" d
    else return d
        
closeConnection :: Plist -> IO ()
closeConnection d = send d $ exit

-- only for tests that write then read
withPlistConnection :: Bool -> (Plist -> IO ()) -> IO ()
withPlistConnection audit 
                    = guardPlistBuddyException 
                    . bracket (openConnection audit)
                              closeConnection

guardPlistBuddyException :: IO a -> IO a
guardPlistBuddyException m = m `catch` \ (PlistBuddyException msg) -> do
       IO.putStrLn $ "\ndiscarded: " ++ show msg
       discard

main :: IO ()
main = hspec $ do
  beforeAll clearDB $ do

    describe "inital plist" $ modifyMaxSuccess (\ x -> 100) $ do

      it "check initial dict is an dictionary" $ withPlistConnection False $ \ d -> do
        r0 <- send d $ get []
        r0 `shouldBe` Dict []

      it "check reset to array" $ withPlistConnection False $ \ d -> do
        _ <- send d $ clear (Array [])
        r0 <- send d $ get []
        r0 `shouldBe` Array []
      
      it "check reset to back to an dict" $ withPlistConnection False $ \ d -> do
        _ <- send d $ clear (Array [])
        _ <- send d $ clear (Dict [])
        r0 <- send d $ get []
        r0 `shouldBe` Dict []

      it "check adding a value at top level" $ 
        property $ \ (Label lbl) (OneValue v) audit -> withPlistConnection audit $ \ d -> do
                debug $ ("add val top",lbl,v)
                _ <- send d $ add [lbl] v
                r0 <- send d $ get []
                r0 `shouldBe` Dict [(lbl,v)]

      it "check adding then setting a value at top level" $ 
        property $ \ (Label lbl) (PrimValue v1) audit ->
          forAll (arbitrarySameType v1) $ \ v2 ->
            withPlistConnection audit $ \ d -> do
              debug $ ("add then set top",lbl,v1,v2)
              _ <- send d $ add [lbl] v1
              _ <- send d $ set [lbl] v2
              r0 <- send d $ get []
              r0 `shouldBe` Dict [(lbl,v2)]

      it "populate a DB" $ 
        property $ \ (DictValue v) audit -> withPlistConnection audit $ \ d -> do
          debug $ ("populate",v)
          r0 <- send d $ do
            populateDict v
            get []
          r0 `shouldBe` v

      it "test deeper get" $ 
        property $ \ (DictValue v) audit -> 
          forAll (arbitraryReadPath 0.8 v) $ \ (Path ps,v') -> do
            withPlistConnection audit $ \ d -> do
              send d $ populateDict v
              r0 <- send d $ get ps
              r0 `shouldBe` v'

      it "test deepest get" $ 
        property $ \ (DictValue v) audit -> 
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v') -> do
            withPlistConnection audit $ \ d -> do
              send d $ populateDict v
              r0 <- send d $ get ps
              r0 `shouldBe` v'


      it "test deepest set then get" $ 
        property $ \ (DictValue v) audit -> 
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v1) -> 
            not (null ps) && (case v1 of { Dict {} -> False; Array {} -> False ; _-> True}) ==>
            forAll (arbitrarySameType v1) $ \ v2 ->
              withPlistConnection audit $ \ d -> do
                debug (v1,v2,ps)
                send d $ populateDict v
                send d $ set ps v2
                r0 <- send d $ get ps
                r0 `shouldBe` v2

      it "test delete" $ 
        property $ \ (DictValue v) audit -> 
          forAll (arbitraryReadPath 1.0 v) $ \ (Path ps,v1) -> 
            not (null ps) ==>
              withPlistConnection audit $ \ d -> do
--                print (v1,ps)
                (r1,parent) <- send d $ do
                  populateDict v
                  r1 <- get ps
                  parent <- get (init ps)
                  delete ps
                  return (r1,parent)
                case parent of
                    Dict {} -> do
                        r2 <- send d $ ((Just <$> get ps) `catchPlistError` \ _ -> return Nothing)
                        (r1,r2) `shouldBe` (v1,Nothing)
                    Array xs -> do
                        xs' <- send d $ do
                          Array xs' <- get (init ps)
                          return xs'
                        (r1,length xs) `shouldBe` (v1,length xs' + 1)

      it "check for bad path error handling" $ 
        property $ \ (DictValue v) (Path p) audit -> p `notIn` v ==>  withPlistConnection audit $ \ d -> do
          debug $ ("bad path",v,p)
          r <- (send d $ (do
                  populateDict v
                  get p
                  return False) `catchPlistError` \ e -> do
                    return True)

          r `shouldBe` True


  beforeAll clearDB $ do
    describe "plist modification" $ do  
      it "test save of DB" $ 
        property $ \ (DictValue v) -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          send d $ do
            populateDict v
            save
            exit
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          r0 `shouldBe` v

      it "test save of DB, with changes in between" $ 
        property $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          send d $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
            exit
          d <- openPlist "test.plist"
          r0 <- send d $ get []
          send d $ exit
          r0 `shouldBe` v

      it "test save and revert of DB" $ 
        property $ \ (DictValue v) (DictValue v') -> guardPlistBuddyException $ do
          d <- openPlist "test.plist"
          send d $ clear (Dict [])  -- clear dict
          r0 <- send d $ do
            populateDict v
            save
            clear $ Dict []
            populateDict v'
            revert
            r <- get []
            exit
            return r
          r0 `shouldBe` v            

populateDict :: Value -> PlistBuddy ()
populateDict (Dict xs) = 
   do sequence_ [ populate (Path $ [i]) v | (i,v) <- xs ]
populateDict _ = error "expecting a Dict"
  
populate :: Path -> Value -> PlistBuddy ()
populate (Path ps) val = 
  case val of
    Dict xs -> do
      add ps (Dict [])
      sequence_ [ populate (Path $ ps ++ [i]) v | (i,v) <- xs ]
    Array vs -> do
      add ps (Array [])
      sequence_ [ populate (Path $ ps ++ [pack (show i)]) v | (i,v) <- [0..] `zip` vs ]
    _ -> do
      add ps val


[] `notIn` _        = False
(p:ps) `notIn` (Dict xs) = case lookup p xs of
                        Nothing -> True
                        Just v -> ps `notIn` v 
(p:ps) `notIn` (Array vs) 
                     | T.all isDigit p && not (T.null p) 
                     = case drop (read (unpack p)) vs of
                         [] -> False
                         (v:_) -> ps `notIn` v
_ `notIn` _          = True

{-
    -- TO ADD
        -- try get type error
        
        r <- send d $ ((set ["I1"] (String "foo")>>return "no failed") `catchPlistError` \ msg -> return msg)
        check "check for type error" r $ "set failed: \"Unrecognized Integer Format\""

        _ <- send d $ exit

        d <- openPlist "test.plist"

        now <- getCurrentTime

	send d $ add ["S5"] (Date now)

        Date r0 <- send d $ get ["S5"]
        
        check "check for date storage" (abs (diffUTCTime now r0) < 1) $ True
        _ <- send d $ exit

        d <- debugOn <$> openPlist "test.plist"
-}


check :: (Eq a, Show a) => Text -> a -> a -> IO ()
check msg t1 t2 = if t1 /= t2 then fail ("check failed: " ++ show (msg,t1,t2)) else TIO.putStrLn msg
 
arbitraryValue :: Int -> Gen Value
arbitraryValue n = frequency 
  [(7,(\ (PrimValue v) -> v) <$> arbitrary),
   (2,mysized $ \ n' -> mkDict <$> sequence [ arbitraryDict (n-1) | _ <- [1..n]]),
   (1,mysized $ \ n' -> Array <$> sequence [ arbitraryValue (n-1) | _ <- [1..n]])
  ]
  where mysized k | n == 0    = k 0
                  | otherwise = modSized 8 k


-- removes dup labels
mkDict :: [(Text,Value)] -> Value
mkDict xs = Dict [ (lbl,v) | (lbl,v) <- nub (map fst xs) `zip` map snd xs ]

arbitraryDict :: Int -> Gen (Text,Value)
arbitraryDict n = do
  Label nm <- arbitrary
  v <- arbitraryValue n
  return (nm,v)

arbitraryDate :: Gen UTCTime
arbitraryDate =
  UTCTime <$> ((\ d -> addDays d (fromGregorian 1950 1 1)) -- 2003 10 25
                  <$> choose (0, 85 * 365)  -- dates after 2038 have issues (wordsize?)
                                -- +2
              )
          <*> (fromInteger <$> choose (0,60 * 60 * 24 - 1))
        
arbitraryText :: Gen Text
arbitraryText = modSized 10 $ \ n -> pack <$> (vectorOf n $ elements ('\n':[' '..'~']))

-- 28
arbitraryData :: Gen BS.ByteString
arbitraryData = modSized 32 $ \ n -> BS.pack <$> (vectorOf n $ elements ([0..255]))

instance Eq Value where
  (==) = eqValue

eqValue :: Value -> Value -> Bool
eqValue (String s1) (String s2) = s1 == s2
eqValue (Array a1)  (Array a2)  = a1 == a2
eqValue (Dict d1)   (Dict d2)   = sortBy f d1 == sortBy f d2  -- order should not matter
  where f (a,_) (b,_) = a `compare` b
eqValue (Bool a1)    (Bool a2)   = a1 == a2
eqValue (Real a1)    (Real a2)   = abs (a1 - a2) <= abs ((a1 + a2) / 1e6)
eqValue (Integer a1) (Integer a2) = a1 == a2
eqValue (Date d1)    (Date d2)    = d1 == d2
eqValue (Data d1)    (Data d2)    = d1 == d2
eqValue _ _ = False

---------------------------------------

---------------------------------------
valueShrink :: Value -> [Value]
valueShrink (Dict []) = []
valueShrink (Dict [(lbl,x)]) = [x]
valueShrink (Dict xs) =
    [ Dict (take i xs ++ drop (i + 1) xs)
    | i <- [0..length xs - 1]
    ] ++ 
    [ Dict (map fst xs `zip` vs)
    | vs <- transpose $ fmap valueShrink (map snd xs) 
    ]
valueShrink (Array []) = []
valueShrink (Array [x]) = [x]
valueShrink (Array vs) =
    [ Array (take i vs ++ drop (i + 1) vs)
    | i <- [0..length vs - 1]
    ] ++ 
    [ Array vs
    | vs <- transpose $ map valueShrink vs
    ]
--valueShrink (Date d) = [Date $ addUTCTime (60*60) d,Date $ addUTCTime (60) d,Date $ addUTCTime (1) d]

valueShrink other = []
    

---------------------------------------

arbitrarySameType :: Value -> Gen Value
arbitrarySameType v0 = do
  (OneValue v) <- arbitrary
  if valueType v0 == valueType v
  then return v
  else arbitrarySameType v0 

newtype PrimValue = PrimValue Value -- any primitive
  deriving (Show,Generic)

instance Arbitrary PrimValue where
  arbitrary = PrimValue <$> oneof 
    [ Integer <$> arbitrary
    , String  <$> arbitraryText
    , Bool    <$> arbitrary
    , Real    <$> arbitrary
    , Date    <$> arbitraryDate -- for now
    , Data    <$> arbitraryData 
    ]
  shrink (PrimValue v) = [ PrimValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype OneValue = OneValue Value -- primitive + empty dict or empty array
  deriving (Show,Generic)
  
instance Arbitrary OneValue where
  arbitrary = OneValue <$> arbitraryValue 0
  shrink (OneValue v) = [ OneValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype DeepValue = DeepValue Value -- any value, to any depth
  deriving (Show,Generic)
  
instance Arbitrary DeepValue where
  arbitrary = DeepValue <$> modSized 8 arbitraryValue
  shrink (DeepValue v) = [ DeepValue v' | v' <- valueShrink v, valueType v == valueType v']

newtype DictValue = DictValue Value -- any value, to any depth
  deriving (Show,Generic)

instance Arbitrary DictValue where
  arbitrary = (DictValue . mkDict) <$> (modSized 8 $ \ n -> vectorOf n (modSized 8 arbitraryDict))
  shrink (DictValue v) = [ DictValue v' | v' <- valueShrink v, valueType v == valueType v']
  
newtype Label = Label Text
  deriving (Show,Generic)

instance Arbitrary Label where
  arbitrary = modSized 32 $ \ n -> (Label . pack) <$> sequence
                [ elements (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
                | _ <- [0..n]
                ]

newtype Path = Path [Text] -- non-empty
  deriving (Show,Generic)

instance Arbitrary Path where
  arbitrary = Path <$> (modSized 8 $ \ n -> vectorOf (n+1) ((\ (Label t) -> t) <$> arbitrary))

modSized :: Int -> (Int -> Gen a) -> Gen a
modSized n k = choose (0,n-1) >>= k

newtype ReadPath = ReadPath [Text] -- can be empty, must be valid
  deriving (Show,Generic)

instance Arbitrary ReadPath where
  arbitrary = ReadPath <$> (modSized 8 $ \ n -> vectorOf n ((\ (Label t) -> t) <$> arbitrary))

arbitraryReadPath :: Double -> Value -> Gen (Path,Value)
arbitraryReadPath n v@(Dict xs) = do
    stop <- choose (0,1)
    if (stop > n) || null xs
    then return (Path [],v)
    else do nm <- elements (map fst xs)
            case lookup nm xs of
              Just v' -> do
                (Path ps,vr) <- arbitraryReadPath n v'
                return (Path (nm:ps),vr)
              Nothing -> error "arbitraryReadPath internal error"
arbitraryReadPath n v@(Array vs) = do
    stop <- choose (0,1)
    if (stop > n) || null vs
    then return (Path [],v)
    else do i <- elements [0..(length vs - 1)]
            (Path ps,vr) <- arbitraryReadPath n (vs !! i)
            return (Path (pack (show i):ps),vr)
arbitraryReadPath _ v = return (Path [],v)


compareValue :: Path -> Value -> Value -> String
compareValue (Path ps) v1 v2 | valueType v1 /= valueType v2 = "different types : " ++ show (ps,v1,v2)
compareValue (Path ps) (Dict ds1) (Dict ds2) 
  | nm1 /= nm2 = "different names of fields in dict : " ++ show (ps,nm1,nm2)
  | otherwise = concat [ case (lookup nm ds1,lookup nm ds2) of
                          (Just v1,Just v2) -> compareValue (Path (ps ++ [nm])) v1 v2 
                          _ -> "internal error in dict compare " ++ show ps
                       | nm <- nm1 ]
 where
   nm1 = sort (nub (map fst ds1))
   nm2 = sort (nub (map fst ds2))

compareValue (Path ps) (Array ds1) (Array ds2) 
  | length ds1 /= length ds2 = "different lengths of array : " ++ show (ps,length ds1,length ds2)
  | otherwise = concat [ compareValue (Path (ps ++ [pack (show i)])) d1 d2 | (i,d1,d2) <- zip3 [0..] ds1 ds2 ]
compareValue (Path ps) v1 v2 
  | v1 /= v2 = "different values : " ++ show (ps,v1,v2)
  | otherwise = ""


debug :: Show a => a -> IO ()
debug = const $ return ()
--debug = print

