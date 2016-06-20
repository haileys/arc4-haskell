module Arc4 (
  initState,
  nextKeystreamByte,
  keystream,
  crypt,
) where

import qualified Data.ByteString as B
import Data.Array
import Data.Word8
import Data.Bits

type S = Array Word8 Word8
data Arc4State = Arc4State S Word8 Word8

swapS :: S -> Word8 -> Word8 -> S
swapS s a b = s // [(a, s!b), (b, s!a)]

initState :: B.ByteString -> Arc4State
initState key =
  let keyBytes = take 256 $ cycle $ B.unpack key
      initialS = listArray (0, 255) [0..255]
      keySchedule (s, j) (i, k) =
        let j' = j + s!i + k
        in (swapS s i j', j')
      (state, _) = foldl keySchedule (initialS, 0) $ zip [0..255] keyBytes
  in Arc4State state 0 0

nextKeystreamByte :: Arc4State -> (Arc4State, Word8)
nextKeystreamByte (Arc4State s i j) =
  let i' = i + 1
      j' = j + s!i'
      s' = swapS s i' j'
      k  = s ! (s!i' + s!j')
  in (Arc4State s' i' j', k)

keystream :: Arc4State -> [Word8]
keystream s = let (s', k) = nextKeystreamByte s in k : keystream s'

crypt :: Arc4State -> B.ByteString -> B.ByteString
crypt s text = B.pack $ zipWith xor (keystream s) (B.unpack text)
