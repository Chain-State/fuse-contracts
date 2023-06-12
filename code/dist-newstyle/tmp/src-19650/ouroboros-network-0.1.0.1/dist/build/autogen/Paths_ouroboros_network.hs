{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ouroboros_network (
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
version = Version [0,1,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/bin"
libdir     = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/lib"
dynlibdir  = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/lib"
datadir    = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/share"
libexecdir = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/libexec"
sysconfdir = "/home/harun/.cabal/store/ghc-8.10.7/ouroboros-network-0.1.0.1-6adc38f96af28aeec70b4c2ac2acdc466238f698664392628bcf32fcebcd7f5d/etc"

getBinDir     = catchIO (getEnv "ouroboros_network_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ouroboros_network_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ouroboros_network_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ouroboros_network_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ouroboros_network_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ouroboros_network_sysconfdir") (\_ -> return sysconfdir)




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
