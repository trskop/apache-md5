-- |
-- Module:       Main
-- Description:  Create htpasswd like entry and print it to stdout.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  portable
--
-- Create htpasswd like entry and print it to stdout.
module Main (main)
    where

import Control.Applicative ((<$>))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Monad.Random (evalRandIO, getRandomRs)
    -- MonadRandom package http://hackage.haskell.org/package/MonadRandom/
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (index, pack)
import qualified Data.ByteString.Char8 as C8 (concat, pack, putStrLn, singleton)
    -- bytestring package http://hackage.haskell.org/package/bytestring
import Data.Digest.ApacheMD5 (alpha64, apacheMD5)


htpasswdEntry :: String -> String -> ByteString -> ByteString
htpasswdEntry username password salt = C8.concat
    [ C8.pack username
    , C8.pack ":$apr1$"
    , salt
    , C8.singleton '$'
    , apacheMD5 (C8.pack password) salt
    ]

genSalt :: IO ByteString
genSalt = evalRandIO
    $ BS.pack . map (BS.index alpha64) . take 8 <$> getRandomRs (0, 63)

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [userName, password] ->
            genSalt >>= C8.putStrLn . htpasswdEntry userName password
        _ -> do
            hPutStrLn stderr $ "Usage: " ++ progName ++ " USER_NAME PASSWORD"
            exitFailure
