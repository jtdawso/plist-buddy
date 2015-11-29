{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Encoding as E
import Database.PlistBuddy (recover,AuditTrail(..), Trail(..), hashcode)
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("audit":rest)  -> auditMain rest
    ("replay":rest) -> replayMain rest
    _ -> error $ unlines
               [ "usage:"
               , "   plist-buddy audit audit-file"
               , "   plist-buddy replay audit-file plist-files"
               ]

auditMain :: [String] -> IO ()
auditMain [file] = do
  trails <- recover file
  sequence_ [ do
      TIO.putStr $ E.decodeUtf8 bs
      putStr $ " -> " ++ rjustify 4 (show (length trail))  ++ case bs' of
                Nothing -> "\n"
                Just bs2 -> " -> " ++ if bs' == nxt then "...\n"
                                      else T.unpack (E.decodeUtf8 bs2) ++ "\n"                                              
    | (AuditTrail bs trail bs',nxt) <- trails `zip` ([ Just x | AuditTrail x _ _ <- tail trails ] ++ [Nothing])
    ]
  where rjustify :: Int -> String -> String
        rjustify n txt = take (n - length txt) (cycle " ") ++ txt


replayMain :: [String] -> IO ()
replayMain [auditFile,plistFile] = do
    trails <- recover auditFile
    let fastForward h [] = return ()
{-
        fastForward h (AuditTrail bs ts done : rest) | bs /= h = do
          putStrLn $ "hash codes differ; stopping"
          putStrLn $ "    found: " ++ T.unpack (E.decodeUtf8 h')
          putStrLn $ "expecting: " ++ T.unpack (E.decodeUtf8 t)
        fastForward h (AuditTrail _ ts done : rest) = do
          -- run trail
          
          
          putStrLn $ "@ " ++ T.unpack (E.decodeUtf8 h)
          case [ trail | trail@(AuditTrail bs _ _) <- trails, bs == h] of
            [] -> do
              putStrLn $ " done"
            [AuditTrail _ ts done] -> do
              putStrLn $ " found " ++ show (length ts) ++ " change(s), applying"
              h' <- hashcode auditFile
              case done of
                Nothing -> do
                  putStrLn " done"
                Just t | h' == t -> do
                  putStrLn $ "hash for changes matches, looking for next change"
                  fastForward h'
                Just t -> do
                  putStrLn $ "hash codes for final result differ; stopping; use causion"
                  putStrLn $ "    found: " ++ T.unpack (E.decodeUtf8 h')
                  putStrLn $ "expecting: " ++ T.unpack (E.decodeUtf8 t)
            xs   -> do
              putStrLn $ " too many trails (" ++ show (length xs) ++ ") found"
-}
    h <- hashcode auditFile
    fastForward h []
