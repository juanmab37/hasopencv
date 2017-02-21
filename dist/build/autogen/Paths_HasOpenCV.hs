module Paths_HasOpenCV (
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
version = Version [1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/barufa/Dropbox/Documentos/Materias LCC/ALPI/FINAL/HasOpenCV/.cabal-sandbox/bin"
libdir     = "/home/barufa/Dropbox/Documentos/Materias LCC/ALPI/FINAL/HasOpenCV/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/HasOpenCV-1.0.0-5SQgIa68rnX5zErUhyWZTe"
datadir    = "/home/barufa/Dropbox/Documentos/Materias LCC/ALPI/FINAL/HasOpenCV/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/HasOpenCV-1.0.0"
libexecdir = "/home/barufa/Dropbox/Documentos/Materias LCC/ALPI/FINAL/HasOpenCV/.cabal-sandbox/libexec"
sysconfdir = "/home/barufa/Dropbox/Documentos/Materias LCC/ALPI/FINAL/HasOpenCV/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HasOpenCV_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HasOpenCV_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HasOpenCV_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HasOpenCV_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HasOpenCV_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
