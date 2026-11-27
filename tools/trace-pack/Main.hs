{-# LANGUAGE OverloadedStrings #-}
-- | Packs v1 JSON traces into trace format v2 (see "Trace.V2" and docs/oracle-testing.md, "Trace format v2").
--
-- > trace-pack pack SRC_SET_DIR DST_SET_DIR [--jobs N]
-- > trace-pack unpack FILE.trace [OUT.json]
--
-- @pack@ writes @DST/<name>.trace@ for each v1 trace in SRC, only after checking that decoding the bytes gives the trace
-- back and that the trace rebuilds exactly the JSON it came from. It copies every other file of the set unchanged,
-- writes @DST/teams.json@, and never touches SRC.
module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities, setNumCapabilities)
import Control.Concurrent.MVar
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, replicateM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict', encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.IORef
import Data.List (sort)
import qualified Data.Vector as V
import System.Directory
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath (dropExtension, (</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Trace
import Trace.V2

main :: IO ()
main = do
  args <- getArgs
  case args of
    "pack" : rest -> pack rest
    ["unpack", f] -> unpack f Nothing
    ["unpack", f, out] -> unpack f (Just out)
    _ -> die "usage: trace-pack pack SRC_SET_DIR DST_SET_DIR [--jobs N]\n       trace-pack unpack FILE.trace [OUT.json]"

unpack :: FilePath -> Maybe FilePath -> IO ()
unpack f out = do
  bytes <- BL.readFile f
  case decodeV2 bytes of
    Left e -> die (f ++ ": " ++ e)
    Right t -> maybe (BL.putStr (encode (toV1 t) <> "\n")) (\o -> BL.writeFile o (encode (toV1 t))) out

-- | A v1 trace file: @<profile>-<seed>.json@ (the parity test's rule). The set's list files are not traces.
isTraceName :: String -> Bool
isTraceName n = case reverse n of
  ('n' : 'o' : 's' : 'j' : '.' : c : _) -> isDigit c
  _ -> False

pack :: [String] -> IO ()
pack args = do
  (src, dst, jobs) <- case args of
    [s, d] -> pure (s, d, 1)
    [s, d, "--jobs", n] | not (null n), all isDigit n, read n > (0 :: Int) -> pure (s, d, read n)
    _ -> die "usage: trace-pack pack SRC_SET_DIR DST_SET_DIR [--jobs N]"
  srcOk <- doesDirectoryExist src
  unless srcOk (die ("trace-pack: " ++ src ++ " is not a directory"))
  createDirectoryIfMissing True dst
  same <- (==) <$> canonicalizePath src <*> canonicalizePath dst
  when same (die "trace-pack: SRC and DST are the same directory")
  entries <- sort <$> listDirectory src
  files <- filterM' (doesFileExist . (src </>)) entries
  let traces = [n | n <- files, isTraceName n]
      others = [n | n <- files, not (isTraceName n), n /= "teams.json"]
  forM_ others $ \n -> copyFile (src </>  n) (dst </> n)
  caps <- getNumCapabilities
  when (caps < jobs) (setNumCapabilities jobs)
  failed <- newIORef (Nothing :: Maybe String)
  slotList <- mapM (\n -> (,) n <$> (newEmptyMVar :: IO (MVar (Maybe Value)))) traces
  queue <- newMVar slotList
  done <- newEmptyMVar
  let worker = do
        next <- modifyMVar queue (\q -> pure (case q of [] -> ([], Nothing); j : js -> (js, Just j)))
        case next of
          Nothing -> putMVar done ()
          Just (n, slot) -> do
            stopped <- readIORef failed
            case stopped of
              Just _ -> putMVar slot Nothing
              Nothing -> do
                r <- try (packOne src dst n >>= evaluate)
                case r of
                  Right (Right teams) -> putMVar slot (Just teams)
                  Right (Left e) -> failWith n e slot
                  Left ex -> failWith n ("exception: " ++ show (ex :: SomeException)) slot
            worker
      failWith n e slot = do
        atomicModifyIORef' failed (\old -> (maybe (Just (n ++ ": " ++ e)) Just old, ()))
        putMVar slot Nothing
  replicateM_ jobs (forkIO worker)
  replicateM_ jobs (takeMVar done)
  msg <- readIORef failed
  case msg of
    Just m -> hPutStrLn stderr ("trace-pack: FAILED " ++ m) >> exitFailure
    Nothing -> pure ()
  teams <- mapM (takeMVar . snd) slotList
  let entries' = [object ["trace" .= dropExtension n, "teams" .= t] | ((n, _), Just t) <- zip slotList teams]
  atomicWrite (dst </> "teams.json") (encode (Array (V.fromList entries')))
  putStrLn ("trace-pack: packed " ++ show (length traces) ++ " traces and copied " ++ show (length others) ++ " other files from " ++ src ++ " to " ++ dst)
  hFlush stdout
  where
    filterM' p = fmap concat . mapM (\x -> (\b -> [x | b]) <$> p x)

-- | Pack one trace; the Right is its teams. Nothing is written unless both checks pass.
packOne :: FilePath -> FilePath -> FilePath -> IO (Either String Value)
packOne src dst n = do
  r <- eitherDecodeFileStrict' (src </> n)
  case r of
    Left e -> pure (Left ("not JSON: " ++ e))
    Right v -> case fromV1 v of
      Left e -> pure (Left ("not a v1 trace: " ++ e))
      Right t -> do
        let bytes = encodeV2 t
        case decodeV2 bytes of
          Left e -> pure (Left ("v2 bytes do not decode: " ++ e))
          Right t'
            | t' /= t -> pure (Left "decodeV2 (encodeV2 t) /= t")
            | toV1 t /= v -> pure (Left "toV1 t /= the JSON the trace came from")
            | otherwise -> do
                atomicWrite (dst </> dropExtension n ++ ".trace") bytes
                pure (Right (trTeams t))

-- | Write to a temporary file in the same directory, then rename.
atomicWrite :: FilePath -> BL.ByteString -> IO ()
atomicWrite f bytes = do
  let tmp = f ++ ".tmp"
  BL.writeFile tmp bytes
  renameFile tmp f
