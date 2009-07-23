{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import qualified Data.ByteString as B

plainfile :: B.ByteString
plainfile = $(embedFile "sample/bar")

plaindir :: [(FilePath, B.ByteString)]
plaindir = $(embedDir "sample")

main :: IO ()
main = do
    print plainfile
    print plaindir
