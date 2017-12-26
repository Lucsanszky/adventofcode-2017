#!/usr/bin/env stack
{-
stack
script
--resolver lts-9.17
--package curl
-}

import           Data.List    (group)
import           Network.Curl
    ( CurlOption (CurlCookieFile)
    , curlGetString
    )

parseInts :: String -> [Int]
parseInts = map (\x -> read [x] :: Int)

captcha :: [Int] -> Int
captcha xs =
    foldr (\x -> (+) (head x * (length x - 1))) 0 . group $
    xs ++ [head xs]

altCaptcha :: [Int] -> Int
altCaptcha xs =
    foldr
        (\(x1, x2) ->
              if x1 == x2
                  then (+) (x1 + x2)
                  else (+) 0)
        0
        (zip xs1 xs2)
  where
    (xs1, xs2) = (take half xs, drop half xs)
    half = length xs `div` 2

main :: IO ()
main = do
    (_, s) <-
        curlGetString
            "http://adventofcode.com/2017/day/1/input"
            [CurlCookieFile ".cookie.txt"]
    mapM_
        (\f -> print . f . parseInts $ takeWhile (/= '\n') s)
        [captcha, altCaptcha]
