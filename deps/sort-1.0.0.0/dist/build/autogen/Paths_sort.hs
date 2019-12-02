{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_sort (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-8.0.2/sort-1.0.0.0-9tjeM3MxlKBBFOuks44eCH"
dynlibdir  = "/usr/local/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/usr/local/share/x86_64-linux-ghc-8.0.2/sort-1.0.0.0"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "sort_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "sort_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "sort_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "sort_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sort_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sort_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
