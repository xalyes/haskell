module Main where

import Test.HUnit
import Regexps


main :: IO Counts
main = runTestTT $ TestList [
{-
    TestCase $ assertEqual "(a|b)*"
        ("ab",[[(Empty,7),(Empty,1)],[(Empty,2),(Empty,4)],[(Char 'a',3)],[(Empty,6)],[(Char 'b',5)],[(Empty,6)],[(Empty,1),(Empty,7)],[]],0,[7])
        (generateNfa $ fst $ parse (scan "(a|b)*") Nothing),
    TestCase $ assertEqual "(a*|b*)*"
        ("ab",[[(Empty,11),(Empty,1)],[(Empty,2),(Empty,6)],[(Empty,5),(Empty,3)],[(Char 'a',4)],[(Empty,3),(Empty,5)],[(Empty,10)],[(Empty,9),(Empty,7)],[(Char 'b',8)],[(Empty,7),(Empty,9)],[(Empty,10)],[(Empty,1),(Empty,11)],[]],0,[11])
        (generateNfa $ fst $ parse (scan "(a*|b*)*") Nothing),
    TestCase $ assertEqual "(a|b)*abbc(a|b)*"
        ("abc",[[(Empty,7),(Empty,1)],[(Empty,2),(Empty,4)],[(Char 'a',3)],[(Empty,6)],[(Char 'b',5)],[(Empty,6)],[(Empty,1),(Empty,7)],[(Char 'a',8)],[(Char 'b',9)],[(Char 'b',10)],[(Char 'c',11)],[(Empty,18),(Empty,12)],[(Empty,13),(Empty,15)],[(Char 'a',14)],[(Empty,17)],[(Char 'b',16)],[(Empty,17)],[(Empty,12),(Empty,18)],[]],0,[18])
        (generateNfa $ fst $ parse (scan "(a|b)*abbc(a|b)*") Nothing),
    TestCase $ assertEqual "checkMatch (a|b)* -> ababbababaabba"
        True
        (checkMatch "(a|b)*" "ababbababaabba"),
    TestCase $ assertEqual "checkMatch (a|b)*abbc(a|b)* -> abbabaabbcabababab"
        True
        (checkMatch "(a|b)*abbc(a|b)*" "abbabaabbcabababab"),
    TestCase $ assertEqual "check not match (a|b)*abbc(a|b)* -> abbabaabbabababab"
        False
        (checkMatch "(a|b)*abbc(a|b)*" "abbabaabbabababab"),
    TestCase $ assertEqual "check not match a|b* -> aaa"
        False
        (checkMatch "a|b*" "aaa")
-}
  ]
