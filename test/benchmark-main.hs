{-# LANGUAGE BangPatterns #-}
module Main
    where

import Control.Applicative  ((<$>))
import Control.Monad (replicateM)

import Data.ByteString (ByteString)

import Control.Monad.Random
import Criterion.Config (Config(cfgPerformGC), defaultConfig, ljust)
import Criterion.Main (bench, defaultMainWith, nf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

import Data.Digest.ApacheMD5 (alpha64, apacheMD5)
import Data.Digest.ApacheMD5.Internal (Salt(Salt))


genSalt :: RandT StdGen IO Salt
genSalt = Salt . BS.pack
    <$> replicateM 8 (BS.index alpha64 <$> getRandomR (0, 63))
    -- We know, that salt will be correct since we generate it out of
    -- alpha64, therefore we don't use mkSalt to check it for us.

genPassword :: Int -> RandT StdGen IO String
genPassword len = replicateM len (getRandomR ('!', '~'))

genData :: Int -> RandT StdGen IO (ByteString, Salt)
genData len = do
    s <- genSalt
    p <- C8.pack <$> genPassword len
    return (p, s)

main :: IO ()
main = do
    (!inputData8, _) <- genData' 8
    (!inputData16, _) <- genData' 16
    (!inputData32, _) <- genData' 32
    (!inputData64, _) <- genData' 64
    (!inputData128, _) <- genData' 128
    (!inputData256, _) <- genData' 256
    (!inputData512, _) <- genData' 512

    defaultMainWith defaultConfig{cfgPerformGC = ljust True} (return ())
        [ bench "Random passwords of length 8" $ test inputData8
        , bench "Random passwords of length 16" $ test inputData16
        , bench "Random passwords of length 32" $ test inputData32
        , bench "Random passwords of length 64" $ test inputData64
        , bench "Random passwords of length 128" $ test inputData128
        , bench "Random passwords of length 256" $ test inputData256
        , bench "Random passwords of length 512" $ test inputData512
        ]
  where
    test = nf $ uncurry apacheMD5
    genData' n = getStdGen >>= runRandT (genData n)
