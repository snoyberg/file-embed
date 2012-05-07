{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed

main :: IO ()
main = print $(embedDir "test/sample")
