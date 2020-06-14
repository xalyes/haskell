module Main where

{-# LANGUAGE FlexibleInstances #-}

import Test.HUnit
import Md5

main :: IO Counts
main = runTestTT $ TestList [
    TestCase $ assertEqual ""
        "d41d8cd98f00b204e9800998ecf8427e" (Md5.md5 ""),
    TestCase $ assertEqual ""
        "0cc175b9c0f1b6a831c399e269772661" (Md5.md5 "a"),
    TestCase $ assertEqual ""
        "900150983cd24fb0d6963f7d28e17f72" (Md5.md5 "abc"),
    TestCase $ assertEqual ""
        "f96b697d7cb7938d525a2f31aaf161d0" (Md5.md5 "message digest"),
    TestCase $ assertEqual ""
        "c3fcd3d76192e4007dfb496cca67e13b" (Md5.md5 "abcdefghijklmnopqrstuvwxyz"),
    TestCase $ assertEqual ""
        "d174ab98d277d9f5a5611c2c9f419d9f" (Md5.md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    TestCase $ assertEqual ""
        "57edf4a22be3c955ac49da2e2107b67a" (Md5.md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
  ]