{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.FileEmbed
import Test.HUnit ((@?=))
import System.FilePath ((</>))

main :: IO ()
main = do
    let received = $(embedDir "test/sample")
    received @?=
        [ ("foo", "foo\n")
        , ("bar" </> "baz", "baz\n")
        ]
