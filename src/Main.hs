{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.StorableVector as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import Control.Monad(forM_)
import Foreign.Storable(Storable)
import Data.Function(on)

--correlate :: V.Vector a -> V.Vector a -> Int
vMul2 :: (Num a, Storable a) => V.Vector a -> V.Vector a -> V.Vector a
vMul2 = V.zipWith (*)

vSum :: (Num b, Storable b) => V.Vector b -> b
vSum = V.foldl (+) 0

minLength :: (Num a, Storable a) => V.Vector a -> V.Vector a -> Int
minLength x y = (min `on` V.length) x y

innerProduct :: (Fractional a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
innerProduct x y = (/ minLen) . vSum $ vMul2 x y
    where minLen = fromIntegral (minLength x y)

normSquared :: (Fractional a, Num a, Storable a) => V.Vector a -> a
normSquared x = innerProduct x x

norm :: (Floating a, Num a, Storable a) => V.Vector a -> a
norm = sqrt . normSquared

correlate :: (Floating a, Num a, Storable a) => V.Vector a -> V.Vector a -> a
correlate x y = innerProduct x' y' / normalizer
    where normalizer = ((*) `on` norm) x' y'
          (x', y') = trim x y

trim :: (Num a, Storable a) =>
        V.Vector a -> V.Vector a -> (V.Vector a, V.Vector a)
trim x y = ((,) `on` (V.take $ minLength x y)) x y

-- slideCorrelate vBig vSmall = map (correlate vSmall) (V.tails vBig)

matchTemplates :: FilePath -> [FilePath] -> IO ()
matchTemplates inPath templatePaths = do
    (info, Just (samplesB :: BV.Buffer Double)) <- SF.readFile inPath
    let samplesV = BV.fromBuffer samplesB
    print $ "Input samples: " ++ (show . V.length $ samplesV)

    forM_ templatePaths $ 
             \templatePath -> do
               (tInfo, Just (tSamplesB :: BV.Buffer Double)) <- SF.readFile templatePath
               let tSamplesV = BV.fromBuffer tSamplesB
                   --temp = V.take 1000 tSamplesV
                   temp = tSamplesV
               print $ "Template '" ++ templatePath ++ "' samples: " ++ (show . V.length $ tSamplesV)
               forM_ [1..50] $ \x -> do 
                                print $ "Correlation " ++ (show x) ++ " - " ++ show (correlate (V.drop x samplesV) temp)
               --print $ "Correlations:" ++ show (take 50 $ slideCorrelate samplesV tSamplesV)

    --let n = V.maximum (V.map abs (BV.fromBuffer x))
    --y = if n == 0 then x else BV.withBuffer (V.map (/n)) x
    --SF.writeFile info outPath y
    return ()


main :: IO ()
main = matchTemplates "test.ogg" ["test.ogg"]