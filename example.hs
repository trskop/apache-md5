-- |
-- Module:       Main
-- Description:  Create htpasswd like entry and print it to stdout.
-- Copyright:    (c) 2013, 2014 Peter Trsko
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
import Data.Digest.ApacheMD5 (Salt, alpha64, apacheMD5, mkSalt, unSalt)


htpasswdEntry :: String -> String -> Salt -> ByteString
htpasswdEntry username password salt = C8.concat
    [ C8.pack username
    , C8.pack ":$apr1$"
    , unSalt salt
    , C8.singleton '$'
    , apacheMD5 (C8.pack password) salt
    ]

genSalt :: IO Salt
genSalt = do
    Just s <- evalRandIO $ mkSalt . BS.pack . map (BS.index alpha64) . take 8
        <$> getRandomRs (0, 63)
        -- We know that Salt is correctly generated, since we use alpha64 to do
        -- it. That is the reason why we can pattern match on Just.
        --
        -- Other option would be to use Salt value constructor from
        -- Data.Digest.ApacheMD5.Internal module.
    return s

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
