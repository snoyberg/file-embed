{-# LANGUAGE TemplateHaskell #-}
module Data.FileEmbed
    ( embedFile
    , embedDir
    ) where

import Language.Haskell.TH (runQ,
                            Exp(AppE, ListE, LitE, TupE),
                            Lit(IntegerL, StringL),
                            Q,
                            runIO)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import Control.Monad (filterM)
import qualified Data.ByteString as B
import Control.Arrow ((&&&), second, first)
import Control.Applicative ((<$>))
import Data.Monoid (mappend)

embedFile :: FilePath -> Q Exp
embedFile fp = (runIO $ B.readFile fp) >>= bsToExp

embedDir :: FilePath -> Q Exp
embedDir fp = ListE <$> ((runIO $ fileList fp) >>= mapM pairToExp)

pairToExp :: (FilePath, B.ByteString) -> Q Exp
pairToExp (path, bs) = do
    exp' <- bsToExp bs
    return $! TupE [LitE $ StringL path, exp']

bsToExp :: B.ByteString -> Q Exp
bsToExp bs = do
    pack <- runQ [| B.pack |]
    return $!
        AppE pack .
        ListE .
        map (LitE . IntegerL . fromIntegral) .
        B.unpack $
        bs

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

fileList :: FilePath -> IO [(FilePath, B.ByteString)]
fileList top = map (first tail) <$> fileList' top ""

fileList' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
fileList' realTop top = do
    let prefix1 = top ++ "/"
        prefix2 = realTop ++ prefix1
    allContents <- filter notHidden <$> getDirectoryContents prefix2
    let all' = map (mappend prefix1 &&& mappend prefix2) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second B.readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList' realTop . fst)
    return $ concat $ files : dirs

liftPair2 :: Monad m => (a, m b) -> m (a, b)
liftPair2 (a, b) = b >>= \b' -> return (a, b')
