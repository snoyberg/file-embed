{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (unless)
import Data.FileEmbed
import System.FilePath ((</>))

infix 1 @?=

(@?=) :: (Eq a, Show a) => a -> a -> IO ()
actual @?= expected = unless (actual == expected) (error $ "expected: " ++ show expected ++ "\n but got: " ++ show actual)

main :: IO ()
main = do
    let received = $(embedDir "test/sample")
    received @?=
        [ ("foo", "foo\r\n")
        , ("bar" </> "baz", "baz\r\n")
        ]
    let str = $(embedStringFile "test/sample/foo") :: String
    filter (/= '\r') str @?= "foo\n"
