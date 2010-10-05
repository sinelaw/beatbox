{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.StorableVector as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Control.Monad(forM)

import Data.Function(on)

--correlate :: V.Vector a -> V.Vector a -> Int
vMul2 = V.zipWith (*)
vSum = V.foldl (+) 0

innerProduct x y = (/ minLen) . vSum $ vMul2 x y
    where minLen = fromIntegral $ (min `on` V.length) x y

normSquared x = innerProduct x x
norm = sqrt . normSquared

correlate x y = innerProduct x y / normalizer
    where normalizer = ((*) `on` norm) x y

matchTemplates :: FilePath -> [FilePath] -> IO ()
matchTemplates inPath templatePaths = do
    (info, Just (samplesB :: BV.Buffer Double)) <- SF.readFile inPath
    let samplesV = BV.fromBuffer samplesB
    print $ "Input samples: " ++ (show . V.length $ samplesV)

    forM templatePaths $ 
             \templatePath -> do
               (tInfo, Just (tSamplesB :: BV.Buffer Double)) <- SF.readFile templatePath
               let tSamplesV = BV.fromBuffer tSamplesB
               print $ "Template '" ++ templatePath ++ "' samples: " ++ (show . V.length $ tSamplesV)
               print $ "Correlation at 0:" ++ show (correlate samplesV tSamplesV)
    --let n = V.maximum (V.map abs (BV.fromBuffer x))
    --y = if n == 0 then x else BV.withBuffer (V.map (/n)) x
    --SF.writeFile info outPath y
    return ()

--import Data.VectorSpace

--matchedFilter :: 

main = matchTemplates "test.ogg" ["test.ogg"]