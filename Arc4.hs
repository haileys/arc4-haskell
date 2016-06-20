module Arc4 (
  initState,
  nextKeystreamByte,
  keystream,
  crypt,
) where

import qualified Data.ByteString as B
import qualified Data.IntMap.Strict as I
import Data.Word8
import Data.Maybe
import Data.Bits

type S = I.IntMap Word8
data Arc4State = Arc4State S Word8 Word8

lookupS :: Word8 -> S -> Word8
lookupS i s = fromJust $ I.lookup (fromIntegral i) s

swapS :: S -> Word8 -> Word8 -> S
swapS s a b = I.insert (fromIntegral b) (lookupS a s) $ I.insert (fromIntegral a) (lookupS b s) s

keySchedule :: B.ByteString -> S -> Word8 -> Int -> S
keySchedule _ s _ 256 = s
keySchedule k s j i =
    let j' = j + lookupS (fromIntegral i) s + B.index k (mod i $ B.length k)
    in keySchedule k (swapS s (fromIntegral i) j') j' (i + 1)

initState :: B.ByteString -> Arc4State
initState key =
  let initialS = I.fromList $ map (\i -> (i, fromIntegral i)) [0..255]
  in Arc4State (keySchedule key initialS 0 0) 0 0

nextKeystreamByte :: Arc4State -> (Arc4State, Word8)
nextKeystreamByte (Arc4State s i j) =
  let i' = i + 1
      j' = j + lookupS i' s
      s' = swapS s i' j'
      k  = lookupS (lookupS i' s + lookupS j' s) s
  in (Arc4State s' i' j', k)

keystream :: Arc4State -> [Word8]
keystream s = let (s', k) = nextKeystreamByte s in k : keystream s'

crypt :: Arc4State -> B.ByteString -> B.ByteString
crypt s text = B.pack $ zipWith xor (keystream s) (B.unpack text)
