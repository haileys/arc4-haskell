import Data.ByteString (ByteString, unpack)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

import Arc4

utf8 :: String -> ByteString
utf8 = encodeUtf8 . pack

main :: IO ()
main = do
  let key = utf8 "seekrit"
  let state = initState key
  let cleartext = utf8 "Hello world"
  let ciphertext = crypt state cleartext
  print $ unpack ciphertext
