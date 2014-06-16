module Interface.Database where

import           Data.Maybe
import qualified Data.Vector           as V
import           Database.HDBC
import           Database.HDBC.Sqlite3 (connectSqlite3)

queryVar :: String -> String -> FilePath -> IO (V.Vector Double)
queryVar var cut infile = do
  conn <- connectSqlite3 infile
  v <- quickQuery' conn ("SELECT " ++ var ++ " from var where " ++ cut) []
  let rows = map convRow v
  disconnect conn
  return . V.fromList $ map (fromMaybe (-10)) rows
      where convRow :: [SqlValue] -> Maybe Double
            convRow [sqlVal] = (return . fromSql) sqlVal
            convRow _        = Nothing
