module Database.PlistBuddy.Shell where

import Control.Monad.Reader

import System.Process
import System.IO
        
data Shell a = Shell (ReaderT ExecDevice IO a)

data ExecDevice = ExecDevice Handle Handle

exec :: FilePath -> IO ExecDevice
exec fileName = do
    (Just hin, Just hout, _, _) <- 
        createProcess (proc "script" ["-q","/dev/null","/usr/libexec/PlistBuddy",fileName])
--      createProcess (proc "/usr/libexec/PlistBuddy" [fileName])    
            { std_in = CreatePipe 
            , std_out = CreatePipe 
            }

    hSetBuffering hin  NoBuffering
    hSetBinaryMode hin True
    hSetBuffering hout NoBuffering
    hSetBinaryMode hout True

    return $ ExecDevice hin hout

send :: ExecDevice -> Shell a -> IO a
send dev (Shell m) = runReaderT m dev

