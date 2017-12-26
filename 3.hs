#!/usr/bin/env stack
{-
stack
script
--resolver lts-9.17
--package curl
-}

import           Network.Curl
    ( CurlOption (CurlCookieFile)
    , curlGetString
    )

distance :: Int -> Int
distance = (\(x, y) -> abs x + abs y) . incrementX 1 (0, 0)
  where
    incrementX :: Int -> (Int, Int) -> Int -> (Int, Int)
    incrementX _ p 1 = p
    incrementX i (x, y) t
        | x == i && y == -i = incrementX (i + 1) (x, y) t
        | x == i = incrementY i (x, y) t
        | otherwise = incrementX i (x + 1, y) (t - 1)
    incrementY :: Int -> (Int, Int) -> Int -> (Int, Int)
    incrementY _ p 1 = p
    incrementY i (x, y) t
        | y == i = decrementX i (x, y) t
        | otherwise = incrementY i (x, y + 1) (t - 1)
    decrementX :: Int -> (Int, Int) -> Int -> (Int, Int)
    decrementX _ p 1 = p
    decrementX i (x, y) t
        | x == -i = decrementY i (x, y) t
        | otherwise = decrementX i (x - 1, y) (t - 1)
    decrementY :: Int -> (Int, Int) -> Int -> (Int, Int)
    decrementY _ p 1 = p
    decrementY i (x, y) t
        | y == -i = incrementX i (x, y) t
        | otherwise = decrementY i (x, y - 1) (t - 1)

main :: IO ()
main = do
    (_, s) <-
        curlGetString
            "http://adventofcode.com/2017/day/3/input"
            [CurlCookieFile ".cookie.txt"]
    mapM_
        (\f ->
              print . f . (\x -> read x :: Int) $
              takeWhile (/= '\n') s)
        [distance]
