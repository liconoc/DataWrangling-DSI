module Paths_MagicHaskeller (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,9,6,6,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/liconoc/.cabal/bin"
libdir     = "/home/liconoc/.cabal/lib/x86_64-linux-ghc-7.10.3/MagicHaskeller-0.9.6.6.1-DIxsA3bOAfJ0Ay1GXdDQx3"
datadir    = "/home/liconoc/.cabal/share/x86_64-linux-ghc-7.10.3/MagicHaskeller-0.9.6.6.1"
libexecdir = "/home/liconoc/.cabal/libexec"
sysconfdir = "/home/liconoc/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "MagicHaskeller_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "MagicHaskeller_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "MagicHaskeller_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "MagicHaskeller_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "MagicHaskeller_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
