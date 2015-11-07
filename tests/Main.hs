{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}
import Database.PlistBuddy 

import Data.Monoid
import Data.Text(Text)
import Data.Text.IO as TIO

main = do
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
        
        send d $ (set ["I1"] (String "foo"))


check :: Text -> Value -> Value -> IO ()
check msg t1 t2 = if t1 /= t2 then fail ("check failed: " ++ show (msg,t1,t2)) else TIO.putStrLn msg
