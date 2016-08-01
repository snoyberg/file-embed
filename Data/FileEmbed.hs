{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
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
      -- * Relative path manipulation
    , makeRelativeToProject
    ) where

import Language.Haskell.TH.Syntax
    ( Exp (AppE, ListE, LitE, TupE, SigE, VarE)
    , Lit (StringL)
    , Q
    , runIO
    , qLocation, loc_filename
#if MIN_VERSION_template_haskell(2, 7, 0)
    , Quasi(qAddDependentFile)
#endif
    )
import System.Directory (doesDirectoryExist, doesFileExist,
                         getDirectoryContents, canonicalizePath)
import Control.Applicative ((<|>), (<$>))
import Control.Exception (throw, ErrorCall(..))
import Control.Monad (filterM)
import Control.Arrow ((&&&), second)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Data.String (IsString, fromString)
import Prelude as P


-- | Embed a single file in your source code.
--
-- > import Data.String
-- >
-- > myFile :: IsString a => a
-- > myFile = $(embedFile "dirName/fileName")
embedFile :: FilePath -> Q Exp
embedFile fp =
#if MIN_VERSION_template_haskell(2, 7, 0)
    qAddDependentFile fp >>
#endif
  (runIO $ readFile fp) >>= toExp

-- | Embed a single existing string file in your source code
-- out of list a list of paths supplied.
embedOneFileOf :: [FilePath] -> Q Exp
embedOneFileOf  ps =
  (runIO $ readExistingFile ps) >>= \ ( path, content ) -> do
#if MIN_VERSION_template_haskell(2, 7, 0)
    qAddDependentFile path
#endif
    toExp content
  where
    readExistingFile :: [FilePath] -> IO (FilePath, String)
    readExistingFile xs = do
      ys <- filterM doesFileExist xs
      case ys of
        (p:_) -> readFile p >>= \ c -> return ( p, c )
        _ -> throw $ ErrorCall "Cannot find file to embed as resource"

-- | Embed a directory recursively in your source code.
--
-- > import Data.String
-- >
-- > myDir :: IsString a => [(FilePath, a)]
-- > myDir = $(embedDir "dirName")
embedDir :: FilePath -> Q Exp
embedDir fp = do
    typ <- [t| forall a. IsString a => [(FilePath, a)] |]
    e <- ListE <$> ((runIO $ getDir) >>= mapM (pairToExp fp))
    return $ SigE e typ
    where 
        getDir = fileList fp ""

        pairToExp :: FilePath -> (FilePath, String) -> Q Exp
        pairToExp _root (path, bs) = do
#if MIN_VERSION_template_haskell(2, 7, 0)
            qAddDependentFile $ _root ++ '/' : path
#endif
            exp' <- toExp bs
            return $! TupE [LitE $ StringL path, exp']

fileList :: FilePath -> FilePath -> IO [(FilePath, String)]
fileList realTop top = do
    allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
    let all' = map ((top </>) &&& (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>=
             mapM (liftPair2 . second readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>=
            mapM (fileList realTop . fst)
    return $ concat $ files : dirs
    where
        notHidden :: FilePath -> Bool
        notHidden ('.':_) = False
        notHidden _ = True

        liftPair2 :: Monad m => (a, m b) -> m (a, b)
        liftPair2 (a, b) = b >>= \b' -> return (a, b')

toExp :: String -> Q Exp
toExp s =
    return $ VarE 'fromString
      `AppE` LitE (StringL s)

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
-- > $(makeRelativeToProject "data/foo.txt" >>= embedFile)
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
