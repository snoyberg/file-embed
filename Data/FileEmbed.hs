{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-- | This module uses template Haskell. Following is a simplified explanation of usage for those unfamiliar with calling Template Haskell functions.
--
-- The function @embedFile@ in this modules embeds a file into the exceutable
-- that you can use it at runtime. A file is represented as a @ByteString@.
-- However, as you can see below, the type signature indicates a value of type
-- @Q Exp@ will be returned. In order to convert this into a @ByteString@, you
-- must use Template Haskell syntax, e.g.:
--
-- > $(embedFile "myfile.txt")
--
-- This expression will have type @ByteString@. Be certain to enable the
-- TemplateHaskell language extension, usually by adding the following to the
-- top of your module:
--
-- > {-# LANGUAGE TemplateHaskell #-}
module Data.FileEmbed
    ( -- * Embed at compile time
      embedFile
    , embedOneFileOf
    , embedDir
    , getDir
      -- * Inject into an executable
#if MIN_VERSION_template_haskell(2,5,0)
    , dummySpace
#endif
    , inject
    , injectFile
      -- * Internal
    , stringToBs
    , bsToExp
    ) where

import Language.Haskell.TH.Syntax
    ( Exp (AppE, ListE, LitE, TupE, SigE, VarE)
#if MIN_VERSION_template_haskell(2,5,0)
    , Lit (StringL, StringPrimL, IntegerL)
#else
    , Lit (StringL, IntegerL)
#endif
    , Q
    , runIO
#if MIN_VERSION_template_haskell(2,7,0)
    , Quasi(qAddDependentFile)
#endif
    )
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents)
import Control.Exception (throw, ErrorCall(..))
import Control.Monad (filterM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Arrow ((&&&), second)
import Control.Applicative ((<$>))
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>))

-- | Embed a single file in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedFile "dirName/fileName")
embedFile :: FilePath -> Q Exp
embedFile fp =
#if MIN_VERSION_template_haskell(2,7,0)
    qAddDependentFile fp >>
#endif
  (runIO $ B.readFile fp) >>= bsToExp

-- | Embed a single existing file in your source code
-- out of list a list of paths supplied.
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(embedFile' [ "dirName/fileName", "src/dirName/fileName" ])
embedOneFileOf :: [FilePath] -> Q Exp
embedOneFileOf ps =
  (runIO $ readExistingFile ps) >>= \ ( path, content ) -> do
#if MIN_VERSION_template_haskell(2,7,0)
    qAddDependentFile path
#endif
    bsToExp content
  where
    readExistingFile :: [FilePath] -> IO ( FilePath, B.ByteString )
    readExistingFile xs = do
      ys <- filterM doesFileExist xs
      case ys of
        (p:_) -> B.readFile p >>= \ c -> return ( p, c )
        _ -> throw $ ErrorCall "Cannot find file to embed as resource"

-- | Embed a directory recursively in your source code.
--
-- > import qualified Data.ByteString
-- >
-- > myDir :: [(FilePath, Data.ByteString.ByteString)]
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir fp = do
    typ <- [t| [(FilePath, B.ByteString)] |]
    e <- ListE <$> ((runIO $ fileList fp) >>= mapM (pairToExp fp))
    return $ SigE e typ

-- | Get a directory tree in the IO monad.
--
-- This is the workhorse of 'embedDir'
getDir :: FilePath -> IO [(FilePath, B.ByteString)]
getDir = fileList

pairToExp :: FilePath -> (FilePath, B.ByteString) -> Q Exp
pairToExp _root (path, bs) = do
#if MIN_VERSION_template_haskell(2,7,0)
    qAddDependentFile $ _root ++ '/' : path
#endif
    exp' <- bsToExp bs
    return $! TupE [LitE $ StringL path, exp']

bsToExp :: B.ByteString -> Q Exp
#if MIN_VERSION_template_haskell(2, 5, 0)
bsToExp bs =
    return $ VarE 'unsafePerformIO
      `AppE` (VarE 'unsafePackAddressLen
      `AppE` LitE (IntegerL $ fromIntegral $ B8.length bs)
#if MIN_VERSION_template_haskell(2, 8, 0)
      `AppE` LitE (StringPrimL $ B.unpack bs))
#else
      `AppE` LitE (StringPrimL $ B8.unpack bs))
#endif
#else
bsToExp bs = do
    helper <- [| stringToBs |]
    let chars = B8.unpack bs
    return $! AppE helper $! LitE $! StringL chars
#endif

stringToBs :: String -> B.ByteString
stringToBs = B8.pack

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden _ = True

fileList :: FilePath -> IO [(FilePath, B.ByteString)]
fileList top = fileList' top ""

fileList' :: FilePath -> FilePath -> IO [(FilePath, B.ByteString)]
fileList' realTop top = do
    allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
    let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second B.readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList' realTop . fst)
    return $ concat $ files : dirs

liftPair2 :: Monad m => (a, m b) -> m (a, b)
liftPair2 (a, b) = b >>= \b' -> return (a, b')

magic :: String
magic = concat ["fe", "MS"]

sizeLen :: Int
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

#if MIN_VERSION_template_haskell(2,5,0)
dummySpace :: Int -> Q Exp
dummySpace space = do
    let size = padSize space
    let start = magic ++ size
    let chars = LitE $ StringPrimL $
#if MIN_VERSION_template_haskell(2,6,0)
            map (toEnum . fromEnum) $
#endif
            start ++ replicate space '0'
    let len = fromIntegral $ length start + space :: Int
    [| getInner (unsafePerformIO (unsafePackAddressLen len $(return chars))) |]
#endif

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
    after = B.drop size rest'

injectFile :: B.ByteString
           -> FilePath -- ^ template file
           -> FilePath -- ^ output file
           -> IO ()
injectFile inj srcFP dstFP = do
    src <- B.readFile srcFP
    case inject inj src of
        Nothing -> error "Insufficient dummy space"
        Just dst -> B.writeFile dstFP dst
