#!/usr/bin/env stack
{- stack
  script
  --resolver lts-8.2
  --package curl
  --package split
-}

import           Control.Arrow   ((&&&))
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import           Network.Curl    (CurlOption (CurlCookieFile), curlGetString)

parseInts
    :: [String]
    -> [Int]
parseInts
    = map (\ x -> read x :: Int)

checksum
    :: [[Int]]
    -> Int
checksum
    = foldr (\ xs -> (+) (maximum xs - minimum xs)) 0

altChecksum
    :: [[Int]]
    -> Int
altChecksum
    = foldr (\ xs -> (+) (findDiv (sorted xs) [])) 0
  where
    sorted
        = sortBy (flip compare)
    findDiv
        :: [Int]
        -> [Int]
        -> Int
    findDiv (x0 : x1 : xs) ys
        | x0 `mod` x1 == 0
        = x0 `div` x1
        | otherwise
        = findDiv (x0 : xs) (ys ++ [x1])
    findDiv _ ys
        = findDiv ys []

main
    :: IO ()
main
    = do
      (_, s) <- curlGetString
                "http://adventofcode.com/2017/day/2/input"
                [CurlCookieFile ".cookie.txt"]

      mapM_
          (\ f ->
             print
           . f
           . map (parseInts . words)
           . takeWhile (/= "")
           $ splitOn "\n" s
          ) [checksum, altChecksum]
