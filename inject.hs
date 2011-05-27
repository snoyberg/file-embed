{-# LANGUAGE OverloadedStrings #-}
import Data.FileEmbed

main = injectFile "Hello World" "template" "injected"
