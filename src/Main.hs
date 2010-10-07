{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV
import Control.Monad(forM_, forM)
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
minLength x y = (min `on` V.length) x y

innerProduct :: (Fractional a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
innerProduct x y = vSum $ vMul2 x y

normSquared :: (Fractional a, Num a, Storable a) => V.Vector a -> a
normSquared x = innerProduct x x

norm :: (Floating a, Num a, Storable a) => V.Vector a -> a
norm = sqrt . normSquared

correlate :: (Floating a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
correlate x y = innerProduct x' y' / normalizer
    where normalizer = ((*) `on` norm) x' y'
          (x', y') = trim x y

-- non-commutative
correlateNorm1 :: (Floating a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
correlateNorm1 x y = innerProduct x' y' / normalizer
    where normalizer = normSquared x'
          (x', y') = trim x y

trim :: (Num a, Storable a) =>
        V.Vector a -> V.Vector a -> (V.Vector a, V.Vector a)
trim x y = ((,) `on` (V.take $ minLength x y)) x y

overlappingWindows :: (Storable a) => Int -> V.Vector a -> [V.Vector a]
overlappingWindows windowSize vec = (V.take windowSize vec) : rest
    where rest = if V.length vec > windowSize
                 then overlappingWindows windowSize (V.drop 1 vec)
                 else []
                   

slideCorrelate :: (Storable a, Floating a) => Int -> V.Vector a -> V.Vector a -> [a]
slideCorrelate samplesNum vBig vSmall = map (correlate vSmall) (overlappingWindows minLen vBig')
    where vBig' = V.take totalSamples vBig
          totalSamples = samplesNum + minLen
          minLen = minLength vBig vSmall


testChunk :: (Storable a, Floating a, Ord a) => a -> V.Vector a -> V.Vector a -> Int -> Int -> (Int, a)
testChunk threshold targetV templateV skipSize windowNum = 
    traceId (startSampleNum, res)
    -- if res > threshold 
    --    then traceName "thres passed" (startSampleNum, foldr (max . abs) 0 correlations)
    --    else (startSampleNum, res)

    where startSampleNum = windowNum * skipSize
          curSamples = V.drop startSampleNum targetV
          res = abs $ correlate templateV curSamples
          correlations = slideCorrelate skipSize curSamples templateV

 
matchTemplates :: FilePath -> [FilePath] -> IO ()
matchTemplates inPath templatePaths = do
    (info, Just (samplesB :: BV.Buffer Double)) <- SF.readFile inPath
    let samplesV = BV.fromBuffer samplesB
        targetLen = V.length samplesV
        numChunks = floor $ ((fromIntegral targetLen) :: Double) / (fromIntegral skipSamplesNum)
        skipSamplesNum = 100
        threshold = 0

    print $ "Input samples: " ++ (show . V.length $ samplesV)
    print $ "Number of chunks: " ++ show numChunks

    results <- forM templatePaths $ 
             \templatePath -> do
               (tInfo, Just (tSamplesB :: BV.Buffer Double)) <- SF.readFile templatePath
               let tSamplesV = BV.fromBuffer tSamplesB
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