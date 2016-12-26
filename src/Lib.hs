module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.String (fromString)
import qualified Data.HashMap.Strict as H
import qualified Data.Hashable as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Char8 as BC8
import System.IO (hPutStrLn, stderr)
import Data.Scientific (floatingOrInteger)
import Control.Monad ((>>))



extract :: Value -> Object
extract (Object x)  = x
extract _ = undefined

toString :: Value -> T.Text
toString (Bool x) = fromString $ if x then "true" else "false"
toString (Number x) = fromString $ go (floatingOrInteger x) where
  go (Left y)  = show y
  go (Right y) = show y
toString (String x) = fromString "\"" `T.append` x `T.append` fromString "\""
toString Null = T.empty
toString _ = undefined

orderedKeys :: H.HashMap k v -> [k]
orderedKeys = map fst . H.toList

putInOrder :: (Eq k, H.Hashable k) => [k] -> H.HashMap k T.Text -> [T.Text]
putInOrder ks h = map (\ k -> H.lookupDefault T.empty k h) ks

toCsv :: [T.Text] -> T.Text
toCsv = T.intercalate (fromString ",")

printToCsv :: [T.Text] -> IO ()
printToCsv = BC8.putStrLn . TE.encodeUtf8 . toCsv

putErr = hPutStrLn stderr

printHeader :: Show s => [s] -> IO ()
printHeader = mapM_ print

inputToCsv :: C8.ByteString -> IO ()
inputToCsv input = do
  let vs = (map decode . C8.lines) input :: [Maybe Value]
  let okeys = orderedKeys . extract <$> head vs

  maybe
    (putErr "Error parsing the first line")
    (\ headers ->
      printToCsv headers >> mapM_
        (\v -> maybe
          (putErr $ "parsing line " ++ show v)
          printToCsv
          (putInOrder headers <$> (H.map toString . extract <$> v))
        )
        vs
    )
    okeys

someFunc :: IO ()
someFunc = do
  a <- C8.getContents
  inputToCsv a
