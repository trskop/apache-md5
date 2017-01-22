{-# LANGUAGE NoImplicitPrelude #-}
module Main (main)
    where

import Prelude (Show(show), error)

import Control.Arrow (Arrow(second))
import Control.Monad.IO.Class (liftIO)
import Control.Monad
    ( Monad((>>=), return)
    , replicateM
    , replicateM_
    , void
    , when
    )
import Data.Eq (Eq((==), (/=)))
import Data.Function ((.), ($), on)
import Data.Int (Int)
import Data.List
    ( (++)
    , break
    , concat
    , drop
    , dropWhile
    , takeWhile
    , unlines
    )
import Data.String (String)
import System.Exit (ExitCode(..))
import System.IO (IO)

import Control.Monad.Random (RandT, getRandomR, runRandT)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import System.Process (readProcessWithExitCode)
import System.Random (StdGen, getStdGen)

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import Data.Digest.ApacheMD5 (apacheMD5)
import Data.Digest.ApacheMD5.Internal (Salt(Salt))


-- {{{ Settings ---------------------------------------------------------------

numberOfTests :: Int
numberOfTests = 1000

maxPasswordLength :: Int
maxPasswordLength = 255

-- }}} Settings ---------------------------------------------------------------

-- | Execute @htpasswd@ command to generate salt and Apache MD5 hash for given
-- password.
runHtpasswd
    :: String
    -- ^ Password
    -> IO (String, String)
    -- ^ (Salt, Hash)
runHtpasswd password = do
    (ec,out,err) <- readProcessWithExitCode "htpasswd"
        ["-nbm", "foo", password] ""
        --       ^^^^^ Username is irelevant
    when (ec /= ExitSuccess)
        -- Missing command, ...
        . error $ unlines
            [ "Command htpasswd returned: "
            , err
            , show ec
            ]
    return $ parse out
    where
        -- Split "<username>:$apr1$<salt>$<hash>\n" to pair (Salt, Hash)
        parse :: String -> (String, String)
        parse = second (drop 1)      -- ("<salt>","<hash>")
            . break (== '$')         -- ("<salt>","$<hash>")
            . drop 7                 -- "<salt>$<hash>"
            . dropWhile (/= ':')     -- ":$apr1$<salt>$<hash>"
            . takeWhile (/= '\n')    -- "<username>:$apr1$<salt>$<hash>"

-- | Generate random password, run @htpasswd@ to obtain salt and reference
-- Apache MD5, then ran provided function with password and salt as arguments
-- and compare its output to what @htpasswd@ generated.
testApacheMD5 :: (String -> String -> String) -> Int -> Assertion
testApacheMD5 toTest n =
    void $ getStdGen >>= runRandT (replicateM_ n testApacheMD5')
  where
    testApacheMD5' :: RandT StdGen IO ()
    testApacheMD5' = do
        password <- genPassword
        (salt, hash) <- liftIO $ runHtpasswd password

        liftIO $ assertEqual (msg salt password) hash
            $ toTest password salt

    msg :: String -> String -> String
    msg s p = concat
        [ "Hash does not match for salt = "
        , show s
        , " and password "
        , show p
        ]

    -- Generate password of length between 0 and maxPasswordLength.
    genPassword :: RandT StdGen IO String
    genPassword = getRandomR (0, maxPasswordLength)
        >>= (`replicateM` getRandomR ('!', '~'))

main :: IO ()
main = defaultMain
    [ testCase ("apacheMD5 (" ++ show numberOfTests ++ " tests)")
        $ testApacheMD5 apacheMD5' numberOfTests
    ]
  where
    -- Wrap tested function so that it takes Strings and produces String.
    apacheMD5' :: String -> String -> String
    apacheMD5' =
        (C8.unpack .) . ((\password -> apacheMD5 password . Salt) `on` C8.pack)
