{-# LANGUAGE BangPatterns #-}
module Main
    where

import Control.Applicative  ((<$>))
import Control.Arrow ((***), first)
import Control.Monad (replicateM)
import Data.Word (Word8)

import Control.Monad.Random
import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Data.Digest.ApacheMD5 (alpha64, apacheMD5)


genSalt :: RandT StdGen IO [Word8]
genSalt = replicateM 8 $ BS.index alpha64 <$> getRandomR (0, 63)

genPassword :: Int -> RandT StdGen IO String
genPassword len = replicateM len (getRandomR ('!', '~'))

genData :: Int -> RandT StdGen IO ([Word8], String)
genData len = do
    s <- genSalt
    p <- genPassword len
    return (s, p)

main :: IO ()
main = do
    (!inputData8, _) <- genData' 8
    (!inputData16, _) <- genData' 16
    (!inputData32, _) <- genData' 32
    (!inputData64, _) <- genData' 64
    (!inputData128, _) <- genData' 128
    (!inputData256, _) <- genData' 256

    defaultMain
        [ bench "Random passwords of length 8" $ test inputData8
        , bench "Random passwords of length 16" $ test inputData16
        , bench "Random passwords of length 32" $ test inputData32
        , bench "Random passwords of length 64" $ test inputData64
        , bench "Random passwords of length 128" $ test inputData128
        , bench "Random passwords of length 256" $ test inputData256
        ]
  where
    test = nf $ uncurry apacheMD5

    genData' n = getStdGen
        >>= (first (BS.pack *** C8.pack) <$>) . runRandT (genData n)
