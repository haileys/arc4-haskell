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

initState :: B.ByteString -> Arc4State
initState key =
  let keyBytes = take 256 $ cycle $ B.unpack key
      initialS = I.fromList $ map (\i -> (i, fromIntegral i)) [0..255]
      keySchedule (s, j) (i, k) =
        let j' = j + lookupS i s + k
        in (swapS s i j', j')
      (state, _) = foldl keySchedule (initialS, 0) $ zip [0..255] keyBytes
  in Arc4State state 0 0

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
