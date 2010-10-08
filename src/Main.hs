{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import Control.Monad(forM)
import Foreign.Storable(Storable)
import Data.Function(on)

import Debug.Trace(trace)
traceId x = trace (show x) x
traceName str x = trace (str ++ " " ++ show x) x

vMul2 :: (Num a, Storable a) => V.Vector a -> V.Vector a -> V.Vector a
vMul2 = V.zipWith (*)

vSum :: (Num b, Storable b) => V.Vector b -> b
vSum = V.foldl (+) 0

minLength :: (Num a, Storable a) => V.Vector a -> V.Vector a -> Int
minLength = min `on` V.length

innerProduct :: (Fractional a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
innerProduct x y = vSum $ vMul2 x y

normSquared :: (Fractional a, Num a, Storable a) => V.Vector a -> a
normSquared x = innerProduct x x

norm :: (Floating a, Num a, Storable a) => V.Vector a -> a
norm = sqrt . normSquared

mean :: (Floating a, Num a, Storable a) => V.Vector a -> a
mean vec = (vSum vec) / (fromIntegral . V.length $ vec)

correlate :: (Floating a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
correlate x y = innerProduct x' y'
    where (x', y') = trim x y

normalize :: (Num a, Floating a, Storable a) => V.Vector a -> V.Vector a
normalize vec = V.map normalize' vec
    where normalize' x = (x - u) / n
          u = mean vec
          n = norm vec 

trim :: (Num a, Storable a) =>
        V.Vector a -> V.Vector a -> (V.Vector a, V.Vector a)
trim x y = ((,) `on` (V.take $ minLength x y)) x y

overlappingWindows :: (Storable a) => Int -> V.Vector a -> [V.Vector a]
overlappingWindows windowSize vec = (V.take windowSize vec) : rest
    where rest = if V.length vec > windowSize
                 then overlappingWindows windowSize (V.drop 1 vec)
                 else []
                   

slideCorrelate :: (Storable a, Floating a) => Int -> Int -> V.Vector a -> V.Vector a -> [a]
slideCorrelate numWindows windowSize vBig vSmall = map (correlate vSmall . normalize) (overlappingWindows windowSize vBig')
    where vBig' = V.take totalSamples vBig
          totalSamples = numWindows + windowSize



testChunk :: (Storable a, Floating a, Ord a) => a -> V.Vector a -> V.Vector a -> Int -> Int -> (Int, a)
testChunk threshold targetV templateV skipSize windowNum = 
    if firstResult > threshold 
       then traceName "thres passed" (startSampleNum, foldr (max . abs) 0 allResults)
       else (startSampleNum, firstResult)

    where startSampleNum = windowNum * skipSize
          curSamples = V.drop startSampleNum targetV
          firstWindow = normalize . V.take minLen $ curSamples
          restWindowsHead = V.drop 1 curSamples
          firstResult = abs $ correlate templateV firstWindow
          restResults = slideCorrelate skipSize minLen restWindowsHead templateV
          allResults = firstResult : restResults
          minLen = minLength targetV templateV
 
matchTemplates :: FilePath -> [FilePath] -> IO ()
matchTemplates inPath templatePaths = do
    (info, Just (samplesB :: BV.Buffer Double)) <- SF.readFile inPath
    let samplesV = BV.fromBuffer samplesB
        targetLen = V.length samplesV
        numChunks = floor $ ((fromIntegral targetLen) :: Double) / (fromIntegral skipSamplesNum)
        skipSamplesNum = 10
        threshold = 0.01

    print $ "Input samples: " ++ (show . V.length $ samplesV)
    print $ "Number of chunks: " ++ show numChunks

    results <- forM templatePaths $ 
             \templatePath -> do
               (tInfo, Just (tSamplesB :: BV.Buffer Double)) <- SF.readFile templatePath
               let tSamplesV = BV.fromBuffer $ tSamplesB
                   templateNorm = norm tSamplesV
                   chunkNums = [0..numChunks]
                   results = map (testChunk threshold samplesV tSamplesV skipSamplesNum) chunkNums
                   filteredResults = filter (\(_,res) -> res > threshold) results
                 
               --print $ "Template '" ++ templatePath ++ "' samples: " ++ (show . V.length $ tSamplesV)
               --print filteredResults
               return (templatePath, 
                       foldr (\(y,res) (maxY, maxRes) 
                                  -> if res > maxRes then (y,res) else (maxY, maxRes)) 
                                 (0,0) filteredResults)
    print results
    --let n = V.maximum (V.map abs (BV.fromBuffer x))
    --y = if n == 0 then x else BV.withBuffer (V.map (/n)) x
    --SF.writeFile info outPath y
    return ()


main :: IO ()
main = matchTemplates "test.ogg" ["test-template.ogg"]