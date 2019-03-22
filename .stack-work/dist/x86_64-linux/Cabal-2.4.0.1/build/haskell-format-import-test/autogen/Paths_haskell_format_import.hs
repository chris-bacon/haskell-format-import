{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_format_import (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/bin"
libdir     = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/lib/x86_64-linux-ghc-8.6.4/haskell-format-import-0.1.0.0-4573a2haUYQ2wGQ7FPIE8L-haskell-format-import-test"
dynlibdir  = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/share/x86_64-linux-ghc-8.6.4/haskell-format-import-0.1.0.0"
libexecdir = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/libexec/x86_64-linux-ghc-8.6.4/haskell-format-import-0.1.0.0"
sysconfdir = "/home/chrisb/haskell-format-import/.stack-work/install/x86_64-linux/nightly-2019-03-13/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_format_import_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_format_import_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_format_import_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_format_import_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_format_import_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_format_import_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
