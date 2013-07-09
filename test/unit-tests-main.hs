module Main (main)
    where

import Control.Arrow ((***), second)
import Control.Monad (replicateM, replicateM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Random
import qualified Data.ByteString.Char8 as C8
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Digest.ApacheMD5 (apacheMD5)


-- Settings -------------------------------------------------------------------

numberOfTests :: Int
numberOfTests = 1000

maxPasswordLength :: Int
maxPasswordLength = 255

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
        -- Split "<username>:$apr1$<salt>$<hash>\n" to pair (Salt,Hash)
        parse :: String -> (String, String)
        parse = second (drop 1)      -- ("<salt>","<hash>")
            . break (== '$')         -- ("<salt>","$<hash>")
            . drop 7                 -- "<salt>$<hash>"
            . dropWhile (/= ':')     -- ":$apr1$<salt>$<hash>"
            . takeWhile (/= '\n')    -- "<username>:$apr1$<salt>$<hash>"

testApacheMD5' :: (String -> String -> String) -> Int -> RandT StdGen IO ()
testApacheMD5' toTest n = replicateM_ n $ do
    password <- genPassword
    (salt, hash) <- liftIO $ runHtpasswd password

    liftIO $ assertEqual (msg salt password) hash
        $ toTest password salt
  where
    msg :: String -> String -> String
    msg s p = concat
        [ "Hash does not match for salt = "
        , show s
        , " and password "
        , show p
        ]

    genPassword :: RandT StdGen IO String
    genPassword = getRandomR (0, maxPasswordLength)
        >>= flip replicateM (getRandomR ('!', '~'))

testApacheMD5 :: (String -> String -> String) -> Int -> Assertion
testApacheMD5 f n = void $ getStdGen >>= runRandT (testApacheMD5' f n)

main :: IO ()
main = defaultMain . hUnitTestToTests $ TestList
    [ TestLabel ("apacheMD5 (" ++ show numberOfTests ++ " tests)")
        . TestCase $ testApacheMD5 apacheMD5' numberOfTests
    ]
  where
    apacheMD5' = curry $ C8.unpack . uncurry apacheMD5 . (C8.pack *** C8.pack)
