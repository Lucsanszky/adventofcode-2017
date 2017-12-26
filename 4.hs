#!/usr/bin/env stack
{-
stack
script
--resolver lts-9.17
--package curl
--package split
--package containers
-}

import           Data.List       (group, sort)
import           Data.List.Split (splitOn)
import           Data.Set        (fromList, size)
import           Network.Curl
    ( CurlOption (CurlCookieFile)
    , curlGetString
    )

compareSize :: [String] -> Bool
compareSize ws = length ws == size (fromList ws)

isValid :: String -> Bool
isValid = compareSize . words

altIsValid :: String -> Bool
altIsValid = compareSize . map sort . words

main :: IO ()
main = do
    (_, s) <-
        curlGetString
            "http://adventofcode.com/2017/day/4/input"
            [CurlCookieFile ".cookie.txt"]
    mapM_
        (\f ->
              print .
              sum . map (fromEnum . f) . takeWhile (/= "") $
              splitOn "\n" s)
        [isValid, altIsValid]
