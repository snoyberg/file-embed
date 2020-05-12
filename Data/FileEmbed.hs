{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module uses template Haskell. Following is a simplified explanation of usage for those unfamiliar with calling Template Haskell functions.
--
-- The function @embedFile@ in this modules embeds a file into the executable
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
    , embedDirListing
    , getDir
      -- * Embed as a IsString
    , embedStringFile
    , embedOneStringFileOf
      -- * Inject into an executable
      -- $inject
#if MIN_VERSION_template_haskell(2,5,0)
    , dummySpace
    , dummySpaceWith
#endif
    , inject
    , injectFile
    , injectWith
    , injectFileWith
      -- * Relative path manipulation
    , makeRelativeToProject
      -- * Internal
    , stringToBs
    , bsToExp
    , strToExp
    ) where

import Language.Haskell.TH.Syntax
    ( Exp (AppE, ListE, LitE, TupE, SigE, VarE)
    , Lit (..)
    , Q
    , runIO
    , qLocation, loc_filename
#if MIN_VERSION_template_haskell(2,7,0)
    , Quasi(qAddDependentFile)
#endif
    )
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH ( mkBytes, bytesPrimL )
import qualified Data.ByteString.Internal as B
#endif
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents, canonicalizePath)
import Control.Exception (throw, ErrorCall(..))
import Control.Monad (filterM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Arrow ((&&&), second)
import Control.Applicative ((<$>))
import Data.ByteString.Unsafe (unsafePackAddressLen)
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Data.String (fromString)
import Prelude as P

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
-- > myFile = $(embedOneFileOf [ "dirName/fileName", "src/dirName/fileName" ])
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

-- | Embed a directory listing recursively in your source code.
--
-- > myFiles :: [FilePath]
-- > myFiles = $(embedDirListing "dirName")
--
-- @since 0.0.11
embedDirListing :: FilePath -> Q Exp
embedDirListing fp = do
    typ <- [t| [FilePath] |]
    e <- ListE <$> ((runIO $ fmap fst <$> fileList fp) >>= mapM strToExp)
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
    return $! TupE
#if MIN_VERSION_template_haskell(2,16,0)
      $ map Just
#endif
      [LitE $ StringL path, exp']

bsToExp :: B.ByteString -> Q Exp
#if MIN_VERSION_template_haskell(2, 5, 0)
bsToExp bs =
    return $ VarE 'unsafePerformIO
      `AppE` (VarE 'unsafePackAddressLen
      `AppE` LitE (IntegerL $ fromIntegral $ B8.length bs)
#if MIN_VERSION_template_haskell(2, 16, 0)
      `AppE` LitE (bytesPrimL (
                let B.PS ptr off sz = bs
                in  mkBytes ptr (fromIntegral off) (fromIntegral sz))))
#elif MIN_VERSION_template_haskell(2, 8, 0)
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

-- | Embed a single file in your source code.
--
-- > import Data.String
-- >
-- > myFile :: IsString a => a
-- > myFile = $(embedStringFile "dirName/fileName")
--
-- Since 0.0.9
embedStringFile :: FilePath -> Q Exp
embedStringFile fp =
#if MIN_VERSION_template_haskell(2,7,0)
    qAddDependentFile fp >>
#endif
  (runIO $ P.readFile fp) >>= strToExp

-- | Embed a single existing string file in your source code
-- out of list a list of paths supplied.
--
-- Since 0.0.9
embedOneStringFileOf :: [FilePath] -> Q Exp
embedOneStringFileOf ps =
  (runIO $ readExistingFile ps) >>= \ ( path, content ) -> do
#if MIN_VERSION_template_haskell(2,7,0)
    qAddDependentFile path
#endif
    strToExp content
  where
    readExistingFile :: [FilePath] -> IO ( FilePath, String )
    readExistingFile xs = do
      ys <- filterM doesFileExist xs
      case ys of
        (p:_) -> P.readFile p >>= \ c -> return ( p, c )
        _ -> throw $ ErrorCall "Cannot find file to embed as resource"

strToExp :: String -> Q Exp
#if MIN_VERSION_template_haskell(2, 5, 0)
strToExp s =
    return $ VarE 'fromString
      `AppE` LitE (StringL s)
#else
strToExp s = do
    helper <- [| fromString |]
    return $! AppE helper $! LitE $! StringL s
#endif

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

magic :: B.ByteString -> B.ByteString
magic x = B8.concat ["fe", x]

sizeLen :: Int
sizeLen = 20

getInner :: B.ByteString -> B.ByteString
getInner b =
    let (sizeBS, rest) = B.splitAt sizeLen b
     in case reads $ B8.unpack sizeBS of
            (i, _):_ -> B.take i rest
            [] -> error "Data.FileEmbed (getInner): Your dummy space has been corrupted."

padSize :: Int -> String
padSize i =
    let s = show i
     in replicate (sizeLen - length s) '0' ++ s

#if MIN_VERSION_template_haskell(2,5,0)
-- | Allocate the given number of bytes in the generate executable. That space
-- can be filled up with the 'inject' and 'injectFile' functions.
dummySpace :: Int -> Q Exp
dummySpace = dummySpaceWith "MS"

-- | Like 'dummySpace', but takes a postfix for the magic string.  In
-- order for this to work, the same postfix must be used by 'inject' /
-- 'injectFile'.  This allows an executable to have multiple
-- 'ByteString's injected into it, without encountering collisions.
--
-- Since 0.0.8
dummySpaceWith :: B.ByteString -> Int -> Q Exp
dummySpaceWith postfix space = do
    let size = padSize space
        magic' = magic postfix
        start = B8.unpack magic' ++ size
        magicLen = B8.length magic'
        len = magicLen + sizeLen + space
        chars = LitE $ StringPrimL $
#if MIN_VERSION_template_haskell(2,6,0)
            map (toEnum . fromEnum) $
#endif
            start ++ replicate space '0'
    [| getInner (B.drop magicLen (unsafePerformIO (unsafePackAddressLen len $(return chars)))) |]
#endif

-- | Inject some raw data inside a @ByteString@ containing empty, dummy space
-- (allocated with @dummySpace@). Typically, the original @ByteString@ is an
-- executable read from the filesystem.
inject :: B.ByteString -- ^ bs to inject
       -> B.ByteString -- ^ original BS containing dummy
       -> Maybe B.ByteString -- ^ new BS, or Nothing if there is insufficient dummy space
inject = injectWith "MS"

-- | Like 'inject', but takes a postfix for the magic string.
--
-- Since 0.0.8
injectWith :: B.ByteString -- ^ postfix of magic string
           -> B.ByteString -- ^ bs to inject
           -> B.ByteString -- ^ original BS containing dummy
           -> Maybe B.ByteString -- ^ new BS, or Nothing if there is insufficient dummy space
injectWith postfix toInj orig =
    if toInjL > size
        then Nothing
        else Just $ B.concat [before, magic', B8.pack $ padSize toInjL, toInj, B8.pack $ replicate (size - toInjL) '0', after]
  where
    magic' = magic postfix
    toInjL = B.length toInj
    (before, rest) = B.breakSubstring magic' orig
    (sizeBS, rest') = B.splitAt sizeLen $ B.drop (B8.length magic') rest
    size = case reads $ B8.unpack sizeBS of
            (i, _):_ -> i
            [] -> error $ "Data.FileEmbed (inject): Your dummy space has been corrupted. Size is: " ++ show sizeBS
    after = B.drop size rest'

-- | Same as 'inject', but instead of performing the injecting in memory, read
-- the contents from the filesystem and write back to a different file on the
-- filesystem.
injectFile :: B.ByteString -- ^ bs to inject
           -> FilePath -- ^ template file
           -> FilePath -- ^ output file
           -> IO ()
injectFile = injectFileWith "MS"

-- | Like 'injectFile', but takes a postfix for the magic string.
--
-- Since 0.0.8
injectFileWith :: B.ByteString -- ^ postfix of magic string
               -> B.ByteString -- ^ bs to inject
               -> FilePath -- ^ template file
               -> FilePath -- ^ output file
               -> IO ()
injectFileWith postfix inj srcFP dstFP = do
    src <- B.readFile srcFP
    case injectWith postfix inj src of
        Nothing -> error "Insufficient dummy space"
        Just dst -> B.writeFile dstFP dst

{- $inject

The inject system allows arbitrary content to be embedded inside a Haskell
executable, post compilation. Typically, file-embed allows you to read some
contents from the file system at compile time and embed them inside your
executable. Consider a case, instead, where you would want to embed these
contents after compilation. Two real-world examples are:

* You would like to embed a hash of the executable itself, for sanity checking in a network protocol. (Obviously the hash will change after you embed the hash.)

* You want to create a self-contained web server that has a set of content, but will need to update the content on machines that do not have access to GHC.

The typical workflow use:

* Use 'dummySpace' or 'dummySpaceWith' to create some empty space in your executable

* Use 'injectFile' or 'injectFileWith' from a separate utility to modify that executable to have the updated content.

The reason for the @With@-variant of the functions is for cases where you wish
to inject multiple different kinds of content, and therefore need control over
the magic key. If you know for certain that there will only be one dummy space
available, you can use the non-@With@ variants.

-}

-- | Take a relative file path and attach it to the root of the current
-- project.
--
-- The idea here is that, when building with Stack, the build will always be
-- executed with a current working directory of the root of the project (where
-- your .cabal file is located). However, if you load up multiple projects with
-- @stack ghci@, the working directory may be something else entirely.
--
-- This function looks at the source location of the Haskell file calling it,
-- finds the first parent directory with a .cabal file, and uses that as the
-- root directory for fixing the relative path.
--
-- @$(makeRelativeToProject "data/foo.txt" >>= embedFile)@
--
-- @since 0.0.10
makeRelativeToProject :: FilePath -> Q FilePath
makeRelativeToProject rel = do
    loc <- qLocation
    runIO $ do
        srcFP <- canonicalizePath $ loc_filename loc
        mdir <- findProjectDir srcFP
        case mdir of
            Nothing -> error $ "Could not find .cabal file for path: " ++ srcFP
            Just dir -> return $ dir </> rel
  where
    findProjectDir x = do
        let dir = takeDirectory x
        if dir == x
            then return Nothing
            else do
                contents <- getDirectoryContents dir
                if any isCabalFile contents
                    then return (Just dir)
                    else findProjectDir dir

    isCabalFile fp = takeExtension fp == ".cabal"
