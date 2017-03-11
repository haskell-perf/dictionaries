{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.DeepSeq
import Data.Function
import Data.List
import System.Environment
import Text.CSV
import Text.Printf

main :: IO ()
main = do
  fp:_ <- getArgs
  reportFromCsv fp

reportFromCsv :: FilePath -> IO ()
reportFromCsv fp = do
  result <- parseCSVFromFile fp
  case result of
    Right (_:rows) -> do
      !readme <- fmap force (readFile "README.md")
      let sep = "<!-- RESULTS -->"
          before = unlines (takeWhile (/= sep) (lines readme) ++ [sep ++ "\n"])
      writeFile
        "README.md"
        (before ++
         unlines
           (map
              format
              (filter
                 (not . null . filter (not . null . filter (not . null)))
                 (groupBy (on (==) (takeWhile (/= '/') . concat . take 1)) rows))))
    _ -> error "Couldn't parse csv"

format :: [[String]] -> String
format rows =
  ("## " ++ takeWhile (/= '/') (concat (concat (take 1 (drop 1 rows))))) ++
  "\n\n" ++
  unlines
    [ ("|Name|" ++ intercalate "|" scales ++ "|")
    , "|" ++ concat (replicate (1 + length scales) "---|")
    ] ++
  unlines
    (map
       (\name ->
          "|" ++ name ++ "|" ++ intercalate "|" (valuesByName name) ++ "|")
       (names))
  where
    valuesByName name =
      map
        (\row@(_:mean:_) ->
           let scale = rowScale row
           in float (valuesByScale scale) (read mean))
        (filter ((== name) . rowName) rows)
    valuesByScale scale =
      map (\(_:mean:_) -> read mean) (filter ((== scale) . rowScale) rows)
    names = nub (map rowName rows)
    scales = nub (map rowScale rows)
    rowName row =
      let s =
            takeWhile
              (/= ':')
              (dropWhile (== '/') (dropWhile (/= '/') (concat (take 1 row))))
      in s
    rowScale row =
      let scale = dropWhile (== ':') (dropWhile (/= ':') (concat (take 1 row)))
      in scale

float :: [Double] -> Double -> String
float others x = let (scale, ext) = secs (mean others)
                 in with (x * scale) ext

-- | Convert a number of seconds to a string.  The string will consist
-- of four decimal places, followed by a short description of the time
-- units.
secs :: Double -> (Double, String)
secs k
    | k >= 1     = 1    `pair` "s"
    | k >= 1e-3  = 1e3  `pair` "ms"
    | k >= 1e-6  = 1e6  `pair` "μs"
    | k >= 1e-9  = 1e9  `pair` "ns"
    | k >= 1e-12 = 1e12 `pair` "ps"
    | k >= 1e-15 = 1e15 `pair` "fs"
    | k >= 1e-18 = 1e18 `pair` "as"
    | otherwise = error "Bad scale"
  where pair= (,)

with :: Double -> String -> String
with (t :: Double) (u :: String)
    | t >= 1e9  = printf "%.4g %s" t u
    | t >= 1e3  = printf "%.0f %s" t u
    | t >= 1e2  = printf "%.1f %s" t u
    | t >= 1e1  = printf "%.2f %s" t u
    | otherwise = printf "%.3f %s" t u

-- | Simple rolling average.
mean :: [Double] -> Double
mean =
    snd .
    foldr
        (\x (cnt,avg) ->
              ( cnt + 1
              , (x + avg * cnt) / (cnt + 1)))
        (0, 0)
