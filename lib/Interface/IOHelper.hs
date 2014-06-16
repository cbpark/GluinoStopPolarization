module Interface.IOHelper where

import           Control.Exception (catch, throwIO)
import           System.Directory  (removeFile)
import           System.IO.Error   (isDoesNotExistError)

removeIfExists :: FilePath -> IO ()
removeIfExists f = removeFile f `catch` handleExists
  where handleExists e | isDoesNotExistError e = return ()
                       | otherwise             = throwIO e
