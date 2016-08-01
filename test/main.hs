{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad
import Data.FileEmbed
import qualified Data.ByteString as B
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath ((</>))

getDir :: FilePath -> IO [(FilePath, B.ByteString)]
getDir top = getDir' top ""
    where
        notHidden :: FilePath -> Bool
        notHidden ('.':_) = False
        notHidden _ = True

        liftPair2 :: Monad m => (a, m b) -> m (a, b)
        liftPair2 (a, b) = b >>= \b' -> return (a, b')

        getDir' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
        getDir' realTop top = do
            allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
            let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
            files <- filterM (doesFileExist . snd) all' >>=
                    mapM (liftPair2 . second B.readFile)
            dirs <- filterM (doesDirectoryExist . snd) all' >>=
                    mapM (getDir' realTop . fst)
            return $ concat $ files : dirs

shouldReturn' :: (Show a, Eq a) => a -> IO a -> Expectation
shouldReturn' = flip shouldReturn

main :: IO ()
main = hspec . describe "Data.FileEmbed" $ do
    describe "embedFile" $ do
        it "handles text files" $ ($(embedFile "test/sample/bar") :: B.ByteString) `shouldReturn'` B.readFile "test/sample/bar"
        it "handles binary files" $ ($(embedFile "test/sample/binary") :: B.ByteString) `shouldReturn'` B.readFile "test/sample/binary"
        
    describe "embedDir" $ do
        let sampleDir = $(embedDir "test/sample") :: [(FilePath, B.ByteString)]
        correct <- runIO $ Main.getDir "test/sample"
        it "handles text and binary files" $ sampleDir `shouldBe` correct
