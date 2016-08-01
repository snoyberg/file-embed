{-# LANGUAGE OverloadedStrings #-}
import Data.FileEmbed.Inject

main = injectFile "Hello World" "template" "injected"
