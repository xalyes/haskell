module Md5 where

{-# LANGUAGE FlexibleInstances #-}

import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Word
import Debug.Trace

md5 :: String -> String
md5 str = let
        (a, b, c, d) = md5Internal (splitEvery 32 (prepareString str)) (toBits a00) (toBits b00) (toBits c00) (toBits d00) 0 (toBits a00) (toBits b00) (toBits c00) (toBits d00)
        result = ([showHex' (fromBits i) | i <- reverse $ splitEvery 8 a] ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 b]
         ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 c] ++ [showHex' (fromBits i) | i <- reverse $ splitEvery 8 d])
    in foldl (.) ("" ++) result $ ""

showHex' :: (Integral a, Show a) => a -> ShowS
showHex' x = if x <= 15 then (("0" ++ showHex x "") ++) else showHex x

convertToLittleEndian :: [[Bool]] -> [[Bool]]
convertToLittleEndian x = map f x where
    f a = foldl (++) [] (reverse $ splitEvery 8 a)

prepareString :: String -> [Bool]
prepareString str =
    let inputStringInBytes = strToBits str in
        addLength (genericLength inputStringInBytes) $ alignBy448 $ foldl (++) [] (convertToLittleEndian (splitEvery 32 (addPadding inputStringInBytes)))

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

addPadding :: [Bool] -> [Bool]
addPadding str     | ((length str) `div` 8) `mod` 4 == 0 = str ++ toBits (0x80::Word8) ++ [False | i <- [0.. 23]]
                   | ((length str) `div` 8) `mod` 4 == 1 = str ++ toBits (0x80::Word8) ++ [False | i <- [0.. 15]]
                   | ((length str) `div` 8) `mod` 4 == 2 = str ++ toBits (0x80::Word8) ++ [False | i <- [0.. 7]]
                   | ((length str) `div` 8) `mod` 4 == 3 = str ++ toBits (0x80::Word8)

strToBits :: String -> [Bool]
strToBits [] = []
strToBits (char:chars) =
    (alignBy8 $ cutZeroBits $ toBits $ ord char) ++ (strToBits chars)

class ConvertedToBits a where
    toBits :: a -> [Bool]

instance ConvertedToBits Word8 where
    toBits x =
        reverse([Data.Bits.testBit x i | i <- [0.. 7]])

instance ConvertedToBits Int where
    toBits x =
        reverse([Data.Bits.testBit x i | i <- [0.. 31]])

instance ConvertedToBits Integer where
    toBits x =
        reverse([Data.Bits.testBit x i | i <- [0.. 63]])

fromBits :: [Bool] -> Int
fromBits x = fromBitsInternal x 0 0
        where fromBitsInternal xx n acc | xx == [] = acc
                                       | otherwise = fromBitsInternal (init xx) (n + 1) (if last xx then (acc + 2^n) else acc)

cutZeroBits :: [Bool] -> [Bool]
cutZeroBits [] = []
cutZeroBits (byte:bytes) =
    if byte == False then cutZeroBits bytes else (byte:bytes)

alignBy8 :: [Bool] -> [Bool]
alignBy8 bytes =
    if length bytes `mod` 8 == 0 then bytes else alignBy8 (False:bytes)

alignBy448 :: [Bool] -> [Bool]
alignBy448 bytes = 
    let l' = bytes ++ [False] in
        if (length l') `mod` 512 == 448
        then l'
        else alignBy448 l'

addLength :: Integer -> [Bool] -> [Bool]
addLength len bytes = 
    let (big, little) = splitAt ((length (l) + 1) `div` 2) l 
        l = toBits len
    in bytes ++ little ++ big

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

class CanModuloSum a where
    (|+) :: [Bool] -> a -> [Bool]

instance CanModuloSum [Bool] where
    (|+) lhs rhs = manyBitsSum lhs rhs False []

instance CanModuloSum Int where
    (|+) lhs rhs = manyBitsSum lhs (toBits rhs) False []

work :: [[Bool]] -> [Bool] -> [Bool] -> [Bool] -> [Bool] -> Int -> Int -> [Bool]
-- work x a b c d s i | traceShow (fromBits (funF b c d)) False = undefined
work x a b c d s i | i < 0 || i > 63 = error "Expected i from 0 to 63"
                   | i < 16 = b |+ (shiftLMultiple (a |+ (funF b c d) |+ (x!!i) |+ (t!!i)) s)
                   | i < 32 = b |+ (shiftLMultiple (a |+ (funG b c d) |+ ((x!!((5*i + 1) `mod` 16)) |+ (t!!(i)))) s)
                   | i < 48 = b |+ (shiftLMultiple (a |+ (funH b c d) |+ ((x!!((3*i + 5) `mod` 16)) |+ (t!!(i)))) s)
                   | i < 64 = b |+ (shiftLMultiple (a |+ (funI b c d) |+ ((x!!((7*i) `mod` 16)) |+ (t!!(i)))) s)

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

shiftLOne :: [Bool] -> [Bool]
shiftLOne (bit:bits) = bits ++ [bit]

shiftLMultiple :: [Bool] -> Int -> [Bool]
-- shiftLMultiple bits count | traceShow (fromBits bits, count) False = undefined
shiftLMultiple bits count =
    if count == 1
    then shiftLOne bits
    else shiftLMultiple (shiftLOne bits) (count - 1)

zipBytesWith3 :: (Bool -> Bool -> Bool -> Bool) -> [Bool] -> [Bool] -> [Bool] -> [Bool]
zipBytesWith3 f x y z = zipWith3Internal f x y z []
    where zipWith3Internal ff xx yy zz acc | length acc == length xx = acc
                                           | otherwise = let 
                                              a = xx !! (length xx - 1 - length acc)
                                              b = yy !! (length yy - 1 - length acc)
                                              c = zz !! (length zz - 1 - length acc)
                                            in zipWith3Internal ff xx yy zz ([ff a b c] ++ acc)

funF :: [Bool] -> [Bool] -> [Bool] -> [Bool]
-- funF xs ys zs | traceShow (fromBits xs, fromBits ys, fromBits zs, fromBits $ zipBytesWith3 (\b c d -> (b && c)) xs ys zs) False = undefined
funF xs ys zs = zipBytesWith3 (\b c d -> (b && c) || ((not b) && d)) xs ys zs

funG :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funG xs ys zs = zipBytesWith3 (\b c d -> (d && b) || ((not d) && c)) xs ys zs

funH :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funH xs ys zs = zipBytesWith3 (\b c d -> (b `xor` c) `xor` d) xs ys zs

funI :: [Bool] -> [Bool] -> [Bool] -> [Bool]
funI xs ys zs = zipBytesWith3 (\b c d -> c `xor` (b || not d)) xs ys zs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

md5Internal :: [[Bool]] -> [Bool] -> [Bool] -> [Bool] -> [Bool] -> Int -> [Bool] -> [Bool] -> [Bool] -> [Bool]
    -> ([Bool], [Bool], [Bool], [Bool])
-- md5Internal x a b c d i a0 b0 c0 d0 | traceShow (fromBits a, fromBits b, fromBits c, fromBits d, i) False = undefined
md5Internal x a b c d 64 a0 b0 c0 d0 = if length x == 16 then (a |+ a0, b |+ b0, c |+ c0, d |+ d0) else md5Internal (drop 16 x) (a |+ a0) (b |+ b0) (c |+ c0) (d |+ d0) 0 (a |+ a0) (b |+ b0) (c |+ c0) (d |+ d0)
md5Internal x a b c d i a0 b0 c0 d0 = md5Internal x d (work x a b c d (s!!i) i) b c (i + 1) a0 b0 c0 d0

