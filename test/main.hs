{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.FileEmbed
import Test.HUnit ((@?=))

main :: IO ()
main = do
    let received = $(embedDir "test/sample")
    received @?= [("foo", "foo\n")]
