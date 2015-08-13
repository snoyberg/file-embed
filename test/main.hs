{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.FileEmbed
import Test.HUnit ((@?=))
import System.FilePath ((</>))

main :: IO ()
main = do
    let received = $(embedDir "test/sample")
    received @?=
        [ ("foo", "foo\r\n")
        , ("bar" </> "baz", "baz\r\n")
        ]
    let str = $(embedStringFile "test/sample/foo") :: String
    str @?= "foo\r\n"
