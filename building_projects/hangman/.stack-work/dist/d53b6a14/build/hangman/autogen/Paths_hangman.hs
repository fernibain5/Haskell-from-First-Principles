{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_hangman (
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

bindir     = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\bin"
libdir     = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\lib\\x86_64-windows-ghc-9.0.2\\hangman-0.1.0.0-DsxAoz1advDA9eRKHRUroM-hangman"
dynlibdir  = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\share\\x86_64-windows-ghc-9.0.2\\hangman-0.1.0.0"
libexecdir = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\libexec\\x86_64-windows-ghc-9.0.2\\hangman-0.1.0.0"
sysconfdir = "C:\\Users\\FERNANDO\\Desktop\\haskell\\haskell-from-first-principles\\building_projects\\hangman\\.stack-work\\install\\6a0fe921\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hangman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hangman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hangman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hangman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hangman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hangman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
