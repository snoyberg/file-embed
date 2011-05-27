{-# LANGUAGE TemplateHaskell #-}
module Data.FileEmbed
    ( -- * Embed at compile time
      embedFile
    , embedDir
    , getDir
      -- * Inject into an executable
    , dummySpace
    , inject
    , injectFile
    ) where

import Language.Haskell.TH (runQ,
                            Exp (AppE, ListE, LitE, TupE),
                            Lit (StringL, StringPrimL, IntegerL),
                            Q,
                            runIO)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import Control.Monad (filterM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Arrow ((&&&), second, first)
import Control.Applicative ((<$>))
import Data.Monoid (mappend)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)

-- | Embed a single file in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedFile "dirName/fileName")
embedFile :: FilePath -> Q Exp
embedFile fp = (runIO $ B.readFile fp) >>= bsToExp

-- | Embed a directory recusrively in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myDir :: [(FilePath, Data.ByteString.ByteString)]
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir fp = ListE <$> ((runIO $ fileList fp) >>= mapM pairToExp)

-- | Get a directory tree in the IO monad.
--
-- This is the workhorse of 'embedDir'
getDir :: FilePath -> IO [(FilePath, B.ByteString)]
getDir = fileList

pairToExp :: (FilePath, B.ByteString) -> Q Exp
pairToExp (path, bs) = do
    exp' <- bsToExp bs
    return $! TupE [LitE $ StringL path, exp']

bsToExp :: B.ByteString -> Q Exp
bsToExp bs = do
    helper <- runQ [| stringToBs |]
    let chars = B8.unpack bs
    return $! AppE helper $! LitE $! StringL chars

stringToBs :: String -> B.ByteString
stringToBs = B8.pack

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

magic = concat ["fe", "MS"]
sizeLen = 20

getInner :: B.ByteString -> B.ByteString
getInner b =
    let (sizeBS, rest) = B.splitAt sizeLen $ B.drop (length magic) b
     in case reads $ B8.unpack sizeBS of
            (i, _):_ -> B.take i rest
            [] -> error "Data.FileEmbed (getInner): Your dummy space has been corrupted."

padSize :: Int -> String
padSize i =
    let s = show i
     in replicate (sizeLen - length s) '0' ++ s

dummySpace :: Int -> Q Exp
dummySpace space = do
    let size = padSize space
    let start = magic ++ size
    let chars = LitE $ StringPrimL $ start ++ replicate space '0'
    let len = LitE $ IntegerL $ fromIntegral $ length start + space
    upi <- [|unsafePerformIO|]
    pack <- [|unsafePackAddressLen|]
    getInner' <- [|getInner|]
    return $ getInner' `AppE` (upi `AppE` (pack `AppE` len `AppE` chars))

inject :: B.ByteString -- ^ bs to inject
       -> B.ByteString -- ^ original BS containing dummy
       -> Maybe B.ByteString -- ^ new BS, or Nothing if there is insufficient dummy space
inject toInj orig =
    if toInjL > size
        then Nothing
        else Just $ B.concat [before, B8.pack magic, B8.pack $ padSize toInjL, toInj, B8.pack $ replicate (size - toInjL) '0', after]
  where
    toInjL = B.length toInj
    (before, rest) = B.breakSubstring (B8.pack magic) orig
    (sizeBS, rest') = B.splitAt sizeLen $ B.drop (length magic) rest
    size = case reads $ B8.unpack sizeBS of
            (i, _):_ -> i
            [] -> error $ "Data.FileEmbed (inject): Your dummy space has been corrupted. Size is: " ++ show sizeBS
    (dummy, after) = B.splitAt size rest'

injectFile :: B.ByteString
           -> FilePath -- ^ template file
           -> FilePath -- ^ output file
           -> IO ()
injectFile inj srcFP dstFP = do
    src <- B.readFile srcFP
    case inject inj src of
        Nothing -> error "Insufficient dummy space"
        Just dst -> B.writeFile dstFP dst
