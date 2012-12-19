module TestTextBuffer where

import TextBuffer

testUpZero = upward (fromString "012\n012", 5) (1, 0) == (0, 0)

testUpNearEnd = upward (fromString "012\n012", 5) (1, 2) == (0, 2)

testUpEnd = upward (fromString "012\n012", 5) (1, 3) == (0, 3)

testUpShorter = upward (fromString "0\n012345", 10) (1, 3) == (0, 1)

testUpEndToShorterEnd = upward (fromString "012\n0123", 5) (1, 4) == (0, 3)

testUpWrap = upward (fromString "012345", 3) (0, 4) == (0, 1)

testUpWrap2 = upward (fromString "012345\n012345", 3) (1, 1) == (0, 4)

testRightEnd = rightward (fromString "012\n012", 5) (0, 2) == (0, 3)

testRightEnd2 = rightward (fromString "012\n012", 5) (0, 3) == (1, 0)

testRightFileEnd = rightward (fromString "012", 5) (0, 3) == (0, 3)

testLeftward = leftward (fromString "012\n012", 5) (1, 0) == (0, 3)
