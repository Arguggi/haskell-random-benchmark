{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Random
import           Control.Monad.State.Strict
import           Criterion
import           Criterion.Main
import qualified Data.Vector                   as V
import qualified Data.Sequence                 as Seq
import qualified System.Random.Mersenne.Pure64 as Mers
import qualified System.Random.MWC             as MWC

rounds :: Int
rounds = 5000

main :: IO ()
main = do
    mersGen <- Mers.newPureMT
    gen <- MWC.createSystemRandom
    defaultMain
    -- Not sure if NF or WHNF is more accurate, lets just test both
        [ bgroup "random-bench NF Data.List"
            [ bench "System Random" $ nfIO systemRandomL
            , bench "MWC Random" $ nfIO (mwcRandomL gen)
            , bench "Mersenne Random" $ nf mersRandomL mersGen
            ]
        , bgroup "random-bench WHNF Data.List"
            [ bench "System Random" $ whnfIO systemRandomL
            , bench "MWC Random" $ whnfIO (mwcRandomL gen)
            , bench "Mersenne Random" $ whnf mersRandomL mersGen
            ]
        , bgroup "random-bench NF Data.Seq"
            [ bench "System Random" $ nfIO systemRandomS
            , bench "MWC Random" $ nfIO (mwcRandomS gen)
            , bench "Mersenne Random" $ nf mersRandomS mersGen
            ]
        , bgroup "random-bench WHNF Data.Seq"
            [ bench "System Random" $ whnfIO systemRandomS
            , bench "MWC Random" $ whnfIO (mwcRandomS gen)
            , bench "Mersenne Random" $ whnf mersRandomS mersGen
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

mwcRandomV :: MWC.GenIO -> IO (V.Vector Int)
mwcRandomV gen = MWC.uniformVector gen rounds

type MersState = State Mers.PureMT Int

newMersRandom :: MersState
newMersRandom = state Mers.randomInt
