{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Criterion
import           Criterion.Main
import qualified Data.Vector                   as V
import qualified Data.Sequence                 as Seq
import           Data.Word
import qualified System.Random.Mersenne.Pure64 as Mers
import qualified System.Random.MWC             as MWC
import qualified System.Random.TF.Init         as TFI
import qualified System.Random.TF.Gen          as TFG

rounds :: Int
rounds = 5000

main :: IO ()
main = do
    mersGen <- Mers.newPureMT
    gen <- MWC.createSystemRandom
    tfGen <- TFI.newTFGen
    defaultMain
    -- Not sure if NF or WHNF is more accurate, lets just test both
        [ bgroup "random-bench NF Data.List"
            [ bench "System Random" $ nfIO systemRandomL
            , bench "MWC Random" $ nfIO (mwcRandomL gen)
            , bench "Mersenne Random" $ nf mersRandomL mersGen
            , bench "TF Random" $ nf tfRandomL tfGen
            ]
        , bgroup "random-bench WHNF Data.List"
            [ bench "System Random" $ whnfIO systemRandomL
            , bench "MWC Random" $ whnfIO (mwcRandomL gen)
            , bench "Mersenne Random" $ whnf mersRandomL mersGen
            , bench "TF Random" $ whnf tfRandomL tfGen
            ]
        , bgroup "random-bench NF Data.Seq"
            [ bench "System Random" $ nfIO systemRandomS
            , bench "MWC Random" $ nfIO (mwcRandomS gen)
            , bench "Mersenne Random" $ nf mersRandomS mersGen
            , bench "TF Random" $ nf tfRandomS tfGen
            ]
        , bgroup "random-bench WHNF Data.Seq"
            [ bench "System Random" $ whnfIO systemRandomS
            , bench "MWC Random" $ whnfIO (mwcRandomS gen)
            , bench "Mersenne Random" $ whnf mersRandomS mersGen
            , bench "TF Random" $ whnf tfRandomS tfGen
            ]
        , bgroup "random-bench NF Data.Vector"
            [ bench "MWC Random" $ nfIO (mwcRandomV gen)
            ]
        , bgroup "random-bench WHNF Data.Vector"
            [ bench "MWC Random" $ whnfIO (mwcRandomV gen)
            ]
        ]

systemRandomL :: IO [Int]
systemRandomL = replicateM rounds getRandom

systemRandomS :: IO (Seq.Seq Int)
systemRandomS = Seq.replicateM rounds getRandom

mwcRandomL :: MWC.GenIO -> IO [Int]
mwcRandomL gen = replicateM rounds (MWC.uniform gen)

mwcRandomS :: MWC.GenIO -> IO (Seq.Seq Int)
mwcRandomS gen = Seq.replicateM rounds (MWC.uniform gen)

mersRandomL :: Mers.PureMT -> [Int]
mersRandomL = evalState (replicateM rounds newMersRandom)

mersRandomS :: Mers.PureMT -> Seq.Seq Int
mersRandomS = evalState (Seq.replicateM rounds newMersRandom)

tfRandomL :: TFG.TFGen -> [Int]
tfRandomL = evalState (replicateM rounds newTFRandom)

tfRandomS :: TFG.TFGen -> Seq.Seq Int
tfRandomS = evalState (Seq.replicateM rounds newTFRandom)

mwcRandomV :: MWC.GenIO -> IO (V.Vector Int)
mwcRandomV gen = MWC.uniformVector gen rounds

type MersState = State Mers.PureMT Int
type TFState = State TFG.TFGen Int

newMersRandom :: MersState
newMersRandom = state Mers.randomInt

newTFRandom :: TFState
newTFRandom = state tfNextInt

tfNextInt :: TFG.TFGen -> (Int, TFG.TFGen)
tfNextInt gen = (int, nextGen)
    where (word, nextGen) = TFG.next gen
          int = toInt word

toInt :: Word32 -> Int
toInt = fromIntegral
