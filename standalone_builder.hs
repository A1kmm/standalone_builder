import System.Process
import System.Environment
import Text.Parsec
import Data.Either
import System.FilePath
import System.Directory
import Control.Monad
import System.IO
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS

parseOpen = do
  string "open(\""
  r <- manyTill anyChar (char '"')
  return r

detectShared =
  try ((try (string ".so") <|> try (string ".dll")) >>
       ((char '.' >> return ()) <|> eof) >> probablyShared) <|>
  (anyChar >> detectShared) <|>
  (eof >> return False)

parseOpenMaybePID =
  ((char '[' >> manyTill anyChar (char ']') >> char ' ' >> return ()) <|> (return ())) >>
  parseOpen

probablyShared = (string "cache" >> return False) <|> return True

rightBoolOrFalse = either (const False) id

isShared = rightBoolOrFalse . parse detectShared ""

untilM :: Monad m => m Bool -> m ()
untilM cond = do
  v <- cond
  if v then untilM cond else return ()

main = do
  (prog:copyDir:_) <- getArgs
  (_, _, stderr) <- readProcessWithExitCode "/usr/bin/strace" ["-f", "-e", "open", prog] ""
  
  print $ rights $ map (parse parseOpenMaybePID "") (lines stderr)
  let opened = filter isShared . rights $ map (parse parseOpenMaybePID "") (lines stderr)
  forM_ opened $ \source -> do
    let (_, basename) = splitFileName source
    let target = copyDir </> basename
    catch (copyFile source target) (const $ return ())
  print opened
