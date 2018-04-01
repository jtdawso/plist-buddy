{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Time
import           Database.PlistBuddy

main :: IO()
main = do
  device <- openPlist "example/example.plist"
  t <- getCurrentTime
  send device $ do
    add ["EnvironmentName"] (String "Dev")
    add ["APIBaseURL"] (String "http://example.com")
    add ["ServiceErrorDomain"] (String "http://error.example.com")
    add ["RTCICEServerURLs"] (Array [])
    add ["RTCICEServerURLs","0"] (String "stun:stun.example.com")
    add ["RTCICEServerURLs","1"] (String "turn:turn.example.com")
    add ["WebSocketAPIVersion"] (Real 0.5)
    add ["allAccessAPIURL"] (Dict [])
    add ["allAccessAPIURL", "ChatEndpoint"] (String "http://chat.example.com")
    add ["allAccessAPIURL", "MappingEndpoint"] (String "http://example.com/map")
    add ["allAccessAPIURL", "ChatPort"] (Integer 3015)
    save
  return ()
