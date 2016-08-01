{-# LANGUAGE TemplateHaskell #-}
import Data.FileEmbed.Inject

main = print $(dummySpace 100)
