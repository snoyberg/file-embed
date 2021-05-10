{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import qualified Data.ByteString as B (ByteString, filter)
import Data.FileEmbed
import System.FilePath ((</>))

infix 1 @?=

(@?=) :: (Eq a, Show a) => a -> a -> IO ()
actual @?= expected = unless (actual == expected) (error $ "expected: " ++ show expected ++ "\n but got: " ++ show actual)

main :: IO ()
main = do
    let received = $(embedDir "test/sample")
    received @?=
        [ ("bar" </> "baz", "baz\r\n")
        , ("foo", "foo\r\n")
        ]
    let str = $(embedStringFile "test/sample/foo") :: String
    filter (/= '\r') str @?= "foo\n"

    let mbs = $(embedFileIfExists "test/sample/foo")
    fmap (B.filter (/= fromIntegral (fromEnum '\r'))) mbs @?= Just "foo\n"

    let mbs2 = $(embedFileIfExists "test/sample/foo2") :: Maybe B.ByteString
    mbs2 @?= Nothing
