{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.PlistBuddy.Shell where

import Control.Monad.Reader
import Data.List

import System.Process
import System.IO
        
newtype Command a = Command (ReaderT ExecDevice IO a)
  deriving (Functor,Applicative,Monad)

data ExecDevice = ExecDevice Handle Handle

exec :: FilePath -> IO ExecDevice
exec fileName = do
    (Just hin, Just hout, _, _) <- 
        -- We use script to force the interactive mode to work
        -- Problem% echo "" | script -q /dev/null cat
 	createProcess (proc "/usr/libexec/PlistBuddy" [fileName])    

--        createProcess (proc "script" ["-q","/dev/null","/usr/libexec/PlistBuddy",fileName])
            { std_in = CreatePipe 
            , std_out = CreatePipe 
            , new_session = True
            }

    hSetBuffering hin  NoBuffering
    hSetBinaryMode hin True
    hSetBuffering hout NoBuffering
    hSetBinaryMode hout True

    print "Here"
    -- Wait for first prompt; ignoring the output
    _ <- untilPrompt hout "\n"

    return $ ExecDevice hin hout

send :: ExecDevice -> Command a -> IO a
send dev (Command m) = runReaderT m dev

prompt :: String
prompt = "\nCommand: "

untilPrompt :: Handle -> String -> IO String
untilPrompt hout cs | reverse prompt `isPrefixOf` cs = return (reverse $ drop (length prompt) $ cs)
                    | otherwise = do
         print "Here1"
         eof <- return False -- hIsEOF hout
         print "Here2"
         if eof 
         then return (reverse cs)
         else do 
            print "Here3"
            c <- hGetChar hout
            print ("Here4",c)
            untilPrompt hout (c : cs)

command :: String -> Command String
command input = Command $ do
    ExecDevice hin hout <- ask
    liftIO $ hPutStrLn hin input  -- send command
    liftIO $ untilPrompt hout "\n"   -- wait for output

{-
(Just hin, Just hout, _, _) <- createProcess (proc "/usr/libexec/PlistBuddy" ["foo.plist"]) { std_in = CreatePipe , std_out = CreatePipe  }
hSetBuffering hin  NoBuffering
hSetBinaryMode hin True
hSetBuffering hout NoBuffering
hSetBinaryMode hout True
-}
