module Md5 where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Word
import Debug.Trace
import Control.Monad.State

data Byte = Byte Bool Bool Bool Bool Bool Bool Bool Bool
    deriving (Show, Eq)
data Byte32 = Byte32 Byte Byte Byte Byte deriving Show
data Byte64 = Byte64 Byte32 Byte32 deriving Show

zeroByte = Byte False False False False False False False False

bools8ToByte :: [Bool] -> Byte
bools8ToByte x = let
    bit0 = x!!7
    bit1 = x!!6
    bit2 = x!!5
    bit3 = x!!4
    bit4 = x!!3
    bit5 = x!!2
    bit6 = x!!1
    bit7 = x!!0
  in Byte bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0

bytesToBools :: [Byte] -> [Bool]
bytesToBools [] = []
bytesToBools (Byte byte7 byte6 byte5 byte4 byte3 byte2 byte1 byte0 : xs) = [byte7, byte6, byte5, byte4, byte3, byte2, byte1, byte0] ++ bytesToBools xs

bools32ToByte32 :: [Bool] -> Byte32
bools32ToByte32 x = let
    byte0 = bools8ToByte (drop 24 x)
    byte1 = bools8ToByte (take 8 (drop 16 x))
    byte2 = bools8ToByte (take 8 (drop 8 x))
    byte3 = bools8ToByte (take 8 (x))
  in Byte32 byte3 byte2 byte1 byte0

word8ToByte :: Word8 -> Byte
word8ToByte x = bools8ToByte $ toBits x

intToByte32 :: Int -> Byte32
intToByte32 x = bools32ToByte32 $ toBits x

integerToByte64 :: Integer -> Byte64
integerToByte64 x = let
    bools = toBits x
    small32 = bools32ToByte32 (drop 32 bools)
    big32 = bools32ToByte32 (take 32 bools)
  in Byte64 big32 small32

md5 :: String -> String
md5 str = let
        (a, b, c, d) = md5Internal (splitEvery 32 (bytesToBools $ prepareString str)) (toBits a00, toBits b00, toBits c00, toBits d00)
        result = ([showHex' (fromBits i) | i <- reverse $ splitEvery 8 a] ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 b]
         ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 c] ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 d])
    in foldl (.) ("" ++) result $ ""

prepareString :: String -> [Byte]
prepareString str =
    let
        inputStringInBytes = strToBytes str
        len = (genericLength inputStringInBytes) * 8
    in
        (addLength len . alignBy448 . convertBytesToLittleEndian . addPadding) inputStringInBytes

md5Step :: State ([[Bool]], Int, [Bool], [Bool], [Bool], [Bool]) ()
md5Step = state $ \(x, i, a, b, c, d) -> ((), (x, i+1, d, (work x a b c d (s!!i) i), b, c))

md5Internal :: [[Bool]] -> ([Bool], [Bool], [Bool], [Bool]) -> ([Bool], [Bool], [Bool], [Bool])
md5Internal input (a0, b0, c0, d0) = let
    (_, _, a, b, c, d) = snd $ runState (replicateM_ 64 md5Step) (input, 0, a0, b0, c0, d0)
    stepResult = (a |+ a0, b |+ b0, c |+ c0, d |+ d0)
  in
    if length input == 16
    then stepResult
    else md5Internal (drop 16 input) stepResult

work :: [[Bool]] -> [Bool] -> [Bool] -> [Bool] -> [Bool] -> Int -> Int -> [Bool]
-- work x a b c d s i | traceShow (fromBits (funF b c d)) False = undefined
work x a b c d s i | i < 0 || i > 63 = error "Expected i from 0 to 63"
                   | i < 16 = b |+ (shiftLMultiple (a |+ (funF b c d) |+ (x!!i) |+ (t!!i)) s)
                   | i < 32 = b |+ (shiftLMultiple (a |+ (funG b c d) |+ ((x!!((5*i + 1) `mod` 16)) |+ (t!!(i)))) s)
                   | i < 48 = b |+ (shiftLMultiple (a |+ (funH b c d) |+ ((x!!((3*i + 5) `mod` 16)) |+ (t!!(i)))) s)
                   | i < 64 = b |+ (shiftLMultiple (a |+ (funI b c d) |+ ((x!!((7*i) `mod` 16)) |+ (t!!(i)))) s)

class ConvertedToBits a where
    toBits :: a -> [Bool]

instance ConvertedToBits Word8 where
    toBits x =
        reverse [Data.Bits.testBit x i | i <- [0.. 7]]

instance ConvertedToBits Int where
    toBits x =
        reverse [Data.Bits.testBit x i | i <- [0.. 31]]

instance ConvertedToBits Integer where
    toBits x =
        reverse [Data.Bits.testBit x i | i <- [0.. 63]]

fromBits :: [Bool] -> Int
fromBits x = fromBitsInternal x 0 0
        where fromBitsInternal xx n acc | xx == [] = acc
                                        | otherwise = fromBitsInternal (init xx) (n + 1) (if last xx then (acc + 2^n) else acc)

showHex' :: (Integral a, Show a) => a -> ShowS
showHex' x =
    if x <= 15
    then (("0" ++ showHex x "") ++)
    else showHex x

convertBytesToLittleEndian :: [Byte] -> [Byte]
convertBytesToLittleEndian x = (foldl (++) []) . (map reverse) . (splitEvery 4) $ x

addPadding :: [Byte] -> [Byte]
addPadding str | (length str) `mod` 4 == 0 = str ++ [word8ToByte 0x80] ++ [zeroByte, zeroByte, zeroByte]
               | (length str) `mod` 4 == 1 = str ++ [word8ToByte 0x80] ++ [zeroByte, zeroByte]
               | (length str) `mod` 4 == 2 = str ++ [word8ToByte 0x80] ++ [zeroByte]
               | (length str) `mod` 4 == 3 = str ++ [word8ToByte 0x80]

strToBytes :: String -> [Byte]
strToBytes [] = []
strToBytes (char:chars) = let
    cutZeroBytes [] = [zeroByte]
    cutZeroBytes (x:xs) = if x == zeroByte then cutZeroBytes xs else (x:xs)
    (Byte32 byte3 byte2 byte1 byte0) = intToByte32 $ ord char
  in (cutZeroBytes [byte3, byte2, byte1, byte0]) ++ strToBytes chars

alignBy448 :: [Byte] -> [Byte]
alignBy448 bytes = 
    let l' = bytes ++ [zeroByte] in
        if ((length l') * 8) `mod` 512 == 448
        then l'
        else alignBy448 l'

addLength :: Integer -> [Byte] -> [Byte]
addLength len bytes = 
    let (Byte64 (Byte32 byte7 byte6 byte5 byte4) (Byte32 byte3 byte2 byte1 byte0)) = integerToByte64 len
    in bytes ++ [byte3, byte2, byte1, byte0, byte7, byte6, byte5, byte4] -- little endian

class CanModuloSum a where
    (|+) :: [Bool] -> a -> [Bool]

instance CanModuloSum [Bool] where
    (|+) lhs rhs = manyBitsSum lhs rhs False []

instance CanModuloSum Int where
    (|+) lhs rhs = manyBitsSum lhs (toBits rhs) False []

oneBitSum :: Bool -> Bool -> Bool -> (Bool, Bool)
oneBitSum overflow a b =
    if overflow == False
    then (a && b, a `xor` b)
    else (a || b, (not (a `xor` b)))

manyBitsSum :: [Bool] -> [Bool] -> Bool -> [Bool] -> [Bool]
-- manyBitsSum lhs rhs overflow acc | traceShow (lhs, rhs, overflow, acc) False = undefined
manyBitsSum lhs rhs overflow acc | length acc == length lhs = acc
    | length acc < length lhs = let
        a = lhs !! ((length lhs) - 1 - (length acc))
        b = rhs !! ((length rhs) - 1 - (length acc))
        (newOverflow, bit) = oneBitSum overflow a b
      in manyBitsSum lhs rhs newOverflow ([bit] ++ acc)

shiftLMultiple :: [Bool] -> Int -> [Bool]
-- shiftLMultiple bits count | traceShow (fromBits bits, count) False = undefined
shiftLMultiple bits count =
    if count == 1
    then shiftLOne bits
    else shiftLMultiple (shiftLOne bits) (count - 1)
  where shiftLOne (bit:bits) = bits ++ [bit]

funF :: [Bool] -> [Bool] -> [Bool] -> [Bool]
-- funF xs ys zs | traceShow (fromBits xs, fromBits ys, fromBits zs, fromBits $ zipWith3 (\b c d -> (b && c)) xs ys zs) False = undefined
funF xs ys zs = zipWith3 (\b c d -> (b && c) || ((not b) && d)) xs ys zs

funG :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funG xs ys zs = zipWith3 (\b c d -> (d && b) || ((not d) && c)) xs ys zs

funH :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funH xs ys zs = zipWith3 (\b c d -> (b `xor` c) `xor` d) xs ys zs

funI :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funI xs ys zs = zipWith3 (\b c d -> c `xor` (b || not d)) xs ys zs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

t = [
    0xd76aa478 :: Int, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
  ]

-- |s specifies the per-round shift amounts
s = [ 7 :: Int, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
  ]

a00 = 0x67452301 :: Int
b00 = 0xefcdab89 :: Int
c00 = 0x98badcfe :: Int
d00 = 0x10325476 :: Int
