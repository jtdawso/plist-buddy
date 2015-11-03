{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
import Database.PlistBuddy 

import Data.Monoid
import Data.Text(Text)
import Data.Text.IO as TIO

main = do
        TIO.writeFile "test.plist" "{}"

        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "initial dict" r0 $ "Dict {\n}"

        _ <- send d $ clear ARRAY
        r1 <- send d $ get []
        check "reset as an array" r1 $ "Array {\n}"

        _ <- send d $ clear DICT
        r2 <- send d $ get []
        check "reset as a dict" r2 $ "Dict {\n}"

        _ <- send d $ clear ARRAY
        r3 <- send d $ get []
        check "reset as an array (2)" r3 $ "Array {\n}"
        send d $ exit

        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "initial dict still there" r0 $ "Dict {\n}"        

        _ <- send d $ clear ARRAY
        r5 <- send d $ get []
        check "reset as an array (3)" r5 $ "Array {\n}"

        send d $ save
        send d $ exit
        
        d <- openPlist "test.plist"        
        
        r6 <- send d $ get []
        check "array after save" r6 $ "Array {\n}"
        
        _ <- send d $ clear DICT
        r7 <- send d $ get []
        check "change to dict" r7 $ "Dict {\n}"        

        send d $ revert -- should revert to the array

        r8 <- send d $ get []
        check "array after save" r8 $ "Array {\n}"
        
        _ <- send d $ clear DICT 
        r9 <- send d $ get []
        check "change to dict" r9 $ "Dict {\n}"    

        _ <- send d $ add ["S1"] STRING Nothing
        _ <- send d $ add ["S2"] STRING (Just "Hello")
        _ <- send d $ add ["I1"] INTEGER Nothing
        _ <- send d $ add ["I2"] INTEGER (Just "123")
        r10 <- send d $ get []
        check "dict with 4 entries" r10 $ 
                "Dict {\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    S2 = Hello\n}"

        _ <- send d $ add ["D1"] DICT Nothing
        _ <- send d $ add ["A1"] ARRAY Nothing
        r10 <- send d $ get []
        check "dict with 6 entries" r10 $  
                "Dict {\n" <>
                "    A1 = Array {\n" <>
                "    }\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    D1 = Dict {\n" <>
                "    }\n" <>
                "    S2 = Hello\n}"
        _ <- send d $ add ["D1","X"] INTEGER (Just "99")
        _ <- send d $ add ["A1",""] INTEGER (Just "1") -- add at end
        _ <- send d $ add ["A1",""] INTEGER (Just "2")  -- add at end
        _ <- send d $ add ["A1","2"] INTEGER (Just "3") -- add at end
        _ <- send d $ add ["A1","0"] INTEGER (Just "4") -- add at start
        r10 <- send d $ get []
        check "dict with 11 entries" r10 $
                "Dict {\n" <>
                "    A1 = Array {\n" <>
                "        4\n" <>
                "        1\n" <>
                "        2\n" <>
                "        3\n" <>
                "    }\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    D1 = Dict {\n" <>
                "        X = 99\n" <>
                "    }\n" <>
                "    S2 = Hello\n}"
        _ <- send d $ delete ["A1","1"] 
        r10 <- send d $ get []
        check "dict with 10 entries" r10 $
                "Dict {\n" <>
                "    A1 = Array {\n" <>
                "        4\n" <>
                "        2\n" <>
                "        3\n" <>
                "    }\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    D1 = Dict {\n" <>
                "        X = 99\n" <>
                "    }\n" <>
                "    S2 = Hello\n}"
        _ <- send d $ delete ["D1"]
        r10 <- send d $ get []
        check "dict with dict removed" r10 $
                "Dict {\n" <>
                "    A1 = Array {\n" <>
                "        4\n" <>
                "        2\n" <>
                "        3\n" <>
                "    }\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    S2 = Hello\n}"

        _ <- send d $ set ["A1","2"] "1234"
        r10 <- send d $ get []
        check "testing set" r10 $
                "Dict {\n" <>
                "    A1 = Array {\n" <>
                "        4\n" <>
                "        2\n" <>
                "        1234\n" <>
                "    }\n" <>
                "    S1 = \n" <>
                "    I2 = 123\n" <>
                "    I1 = 0\n" <>
                "    S2 = Hello\n}"
        _ <- send d $ save
        _ <- send d $ exit
        d <- openPlist "test.plist"

        r0 <- send d $ get []
        check "updated dict still there" r0 $   
                "Dict {\n" <>
                "    S1 = \n" <>
                "    S2 = Hello\n" <>
                "    I1 = 0\n" <>
                "    A1 = Array {\n" <>
                "        4\n" <>
                "        2\n" <>
                "        1234\n" <>
                "    }\n" <>
                "    I2 = 123\n}"
        

                        
check :: Text -> Text -> Text -> IO ()
check msg t1 t2 = if t1 /= t2 then fail ("check failed: " ++ show (msg,t1,t2)) else TIO.putStrLn msg
        
