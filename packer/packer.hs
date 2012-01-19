{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs.Implicit
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files
import qualified Data.Serialize.Builder as S
import Control.Monad
import Data.Bits
import Text.Printf

data PackerOpts = PackerOpts { directory :: String, selfinst :: String, installerout :: String } deriving(Eq, Ord, Data, Typeable)
packerOpts = PackerOpts { directory = def &= help "Directory to pack", selfinst = def &= help "Self-installer to use", 
                          installerout = def &= help "Where to write the installer to" }

buildArchive writeTo archivePath actualPath = do
  contents <- liftM (filter (\v -> v /= "." && v /= "..")) $ getDirectoryContents actualPath
  forM_ contents $ \fn ->
    do
      let fullActualPath = actualPath </> fn
      let fullArchivePath = archivePath </> fn
      let namelen = fromIntegral . length $ fullArchivePath
      fs <- getFileStatus fullActualPath
      let fmode = fromIntegral (fileMode fs)
      let ftype = if isDirectory fs then 1 else if isSymbolicLink fs then 2 else 0
      let filelen = fromIntegral . fileSize $ fs
      putStrLn $ printf "%s %o" fullArchivePath fmode
      let putWithContents contents =
            BS.hPut writeTo . S.toLazyByteString $
              S.putWord32le namelen `S.append`
              S.putWord32le filelen `S.append`
              S.putWord32le (fmode .&. 0xFFF) `S.append`
              S.putWord32le ftype `S.append`
              S.fromLazyByteString (BS.pack fullArchivePath) `S.append`
              S.fromLazyByteString contents
      if namelen > 511 || (isSymbolicLink fs && filelen > 511)
        then
          putStrLn $ "Warning - ignoring " ++ fullArchivePath ++ " - name or symlink is too long"
        else
          if (not (isDirectory fs))
            then
              withFile fullActualPath ReadMode $ \hActualFile ->
                do
                  fcontents <- BS.hGetContents hActualFile
                  putWithContents fcontents
          else do
            putWithContents (BS.pack "")
            buildArchive writeTo fullArchivePath fullActualPath

mainWithOpts (PackerOpts { directory = directory, selfinst = selfinst, 
                           installerout = installerout }) =
  withFile installerout WriteMode $ \hInstaller -> do
    withFile selfinst ReadMode $ \hSelfInst ->
      BS.hGetContents hSelfInst >>= BS.hPut hInstaller
    hPutStrLn hInstaller "MAGICMARKER"
    (xzin, xzout, xzerr, procID) <- runInteractiveCommand ("xz -7 -c -Ccrc32 -")
    (do
        outmvar <- newEmptyMVar
        forkIO $ BS.hGetContents xzout >>= BS.hPut hInstaller >> putMVar outmvar ()
        buildArchive xzin "" directory
        hClose xzin
        waitForProcess procID
        takeMVar outmvar
      ) `finally`  (do
                       hClose xzin
                       hClose xzout
                       hClose xzerr
                   )

main = cmdArgs packerOpts >>= mainWithOpts
