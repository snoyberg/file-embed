{-# LANGUAGE TemplateHaskell #-}
import Test.Hspec
import Test.Hspec.HUnit
import Test.HUnit

import Data.FileEmbed
import qualified Data.ByteString as S
import Data.List (sort)

myeq x y = do
    y' <- y
    x @=? y'

main = hspec $ descriptions
    [ describe "embedFile"
        [ it "handles text files" $ $(embedFile "sample/bar") `myeq` S.readFile "sample/bar"
        , it "handles binary files" $ $(embedFile "sample/binary") `myeq` S.readFile "sample/binary"
        ]
    , describe "getDir"
        [ it "takes all non-hidden files" $ ["bar", "baz", "bin", "binary"] `myeq` (fmap (sort . map fst) $ getDir "sample")
        ]
    , describe "embedDir"
        [ it "handles text and binary files" $ $(embedDir "sample") `myeq` getDir "sample"
        ]
    ]
