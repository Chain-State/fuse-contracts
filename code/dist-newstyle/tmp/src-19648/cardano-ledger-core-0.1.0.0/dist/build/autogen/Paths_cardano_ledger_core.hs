{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cardano_ledger_core (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/bin"
libdir     = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/lib"
dynlibdir  = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/lib"
datadir    = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/share"
libexecdir = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/libexec"
sysconfdir = "/home/harun/.cabal/store/ghc-8.10.7/cardano-ledger-core-0.1.0.0-ac7fd40bcc3c128329d3e59be989c9d29e1b5395358a23376eb7cbadd0832684/etc"

getBinDir     = catchIO (getEnv "cardano_ledger_core_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cardano_ledger_core_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cardano_ledger_core_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cardano_ledger_core_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cardano_ledger_core_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cardano_ledger_core_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
