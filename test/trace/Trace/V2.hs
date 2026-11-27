{-# LANGUAGE OverloadedStrings #-}
-- | Trace format v2: a compact binary encoding of a 'Trace' that loses nothing.
--
-- File layout: the magic @PSTR@, a format version byte (2), then a zlib-compressed body. The body is a per-file
-- string table (every distinct 'Text', once) followed by the trace. Integers are zigzag LEB128 varints; strings are
-- table indices. Each snapshot is a delta against the one before it (the snapshot before step 0 is empty, and @final@
-- follows the last step): a Pokemon equal to the same side's Pokemon with the same @orig@ in the previous snapshot
-- is a back-reference (its orig), otherwise it is written in full; the field, the side conditions and the rest are
-- always written in full. Teams are kept as JSON.
module Trace.V2 (encodeV2, decodeV2) where

import Control.Monad (replicateM, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import qualified Codec.Compression.Zlib as Z
import qualified Codec.Compression.Zlib.Internal as ZI
import Data.Aeson (Value, eitherDecodeStrict', encode)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64, Word8)

import Trace

magic :: B.ByteString
magic = "PSTR"

formatVersion :: Word8
formatVersion = 2

-- ---------------------------------------------------------------------------
-- Encoding

-- | The body writer: the string table so far (text to index) over the byte stream.
type Enc = StateT (Map Text Int) PutM

zigzag :: Int -> Word64
zigzag n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

putVar :: Int -> Put
putVar = go . zigzag
  where
    go w
      | w < 0x80 = putWord8 (fromIntegral w)
      | otherwise = putWord8 (fromIntegral (w .&. 0x7f) .|. 0x80) >> go (w `shiftR` 7)

var :: Int -> Enc ()
var = lift . putVar

byte :: Word8 -> Enc ()
byte = lift . putWord8

flag :: Bool -> Enc ()
flag b = byte (if b then 1 else 0)

str :: Text -> Enc ()
str t = do
  tbl <- get
  case M.lookup t tbl of
    Just i -> var i
    Nothing -> do
      let i = M.size tbl
      put (M.insert t i tbl)
      var i

listOf :: (a -> Enc ()) -> [a] -> Enc ()
listOf f xs = var (length xs) >> mapM_ f xs

maybeOf :: (a -> Enc ()) -> Maybe a -> Enc ()
maybeOf _ Nothing = byte 0
maybeOf f (Just x) = byte 1 >> f x

putDraw :: TDraw -> Enc ()
putDraw (TDraw lo hi r) = var lo >> var hi >> var r

putScalars :: Map Text Scalar -> Enc ()
putScalars m = listOf one (M.toAscList m)
  where
    one (k, SInt i) = str k >> byte 0 >> var i
    one (k, SText t) = str k >> byte 1 >> str t

putConds :: Map Text Cond -> Enc ()
putConds m = listOf one (M.toAscList m)
  where
    one (k, Cond d l) = do
      str k
      byte (maybe 0 (const 1) d .|. maybe 0 (const 2) l)
      mapM_ var d
      mapM_ var l

putMon :: MonSnap -> Enc ()
putMon m = do
  var (mOrig m) >> str (mSpecies m) >> var (mLevel m) >> str (mGender m) >> var (mHp m) >> var (mMaxHp m) >> str (mStatus m)
  putScalars (mStatusState m)
  byte ((if mFainted m then 1 else 0) .|. (if mActive m then 2 else 0))
  let StatsSnap a b c d e = mStats m
      BoostsSnap a' b' c' d' e' f' g' = mBoosts m
  mapM_ var [a, b, c, d, e, a', b', c', d', e', f', g', mSpeed m]
  listOf str (mTypes m)
  str (mTeraType m) >> str (mTerastallized m) >> str (mAbility m) >> str (mItem m)
  listOf (\mv -> str (moId mv) >> var (moPP mv) >> var (moMaxPP mv) >> flag (moDisabled mv)) (mMoves m)
  listOf (\(k, ent) -> str k >> putScalars ent) (M.toAscList (mVolatiles m))

putSide :: SideSnap -> SideSnap -> Enc ()
putSide prev cur = do
  str (ssId cur) >> var (ssLeft cur)
  putConds (ssConds cur)
  listOf one (ssMons cur)
  where
    one m = case find ((== mOrig m) . mOrig) (ssMons prev) of
      Just p | p == m -> byte 0 >> var (mOrig m)
      _ -> byte 1 >> putMon m

putSnap :: Snap -> Snap -> Enc ()
putSnap prev cur = do
  var (snTurn cur) >> str (snRequest cur) >> flag (snEnded cur) >> maybeOf str (snWinner cur)
  let f = snField cur
  str (fsWeather f) >> maybeOf var (fsWeatherDuration f) >> str (fsTerrain f) >> maybeOf var (fsTerrainDuration f)
  putConds (fsPseudo f)
  putSide (snP1 prev) (snP1 cur)
  putSide (snP2 prev) (snP2 cur)

body :: Trace -> Enc ()
body t = do
  var (trVersion t) >> str (trShowdownVersion t) >> str (trFormat t) >> str (trSeed t) >> var (trPolicySeed t)
  byte (case trTera t of Nothing -> 0; Just False -> 1; Just True -> 2)
  flag (trTruncated t)
  let teams = BL.toStrict (encode (trTeams t))
  var (B.length teams)
  lift (putByteString teams)
  var (length (trSteps t))
  let go prev [] = pure prev
      go prev (s : ss) = do
        listOf putDraw (stDraws s)
        putSnap prev (stState s)
        listOf str (fst (stLegal s)) >> listOf str (snd (stLegal s))
        maybeOf str (fst (stChosen s)) >> maybeOf str (snd (stChosen s))
        go (stState s) ss
  lastSnap <- go emptySnap (trSteps t)
  listOf putDraw (trFinalDraws t)
  putSnap lastSnap (trFinal t)

-- | Encode a trace as a v2 file.
encodeV2 :: Trace -> BL.ByteString
encodeV2 t = BL.fromChunks [magic, B.singleton formatVersion] <> Z.compress (runPut (putVar (M.size tbl) >> mapM_ putText ordered) <> bodyBytes)
  where
    (tbl, bodyBytes) = case runPutM (execStateT (body t) M.empty) of
      (m, b) -> (m, b)
    ordered = map fst (sortOn snd (M.toList tbl))
    putText s = let b = TE.encodeUtf8 s in putVar (B.length b) >> putByteString b

-- ---------------------------------------------------------------------------
-- Decoding

type Strs = V.Vector Text

getVar :: Get Int
getVar = go 0 0
  where
    go :: Int -> Word64 -> Get Int
    go sh acc = do
      b <- getWord8
      let acc' = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` sh)
      if b .&. 0x80 /= 0
        then (if sh >= 63 then fail "varint too long" else go (sh + 7) acc')
        else pure (fromIntegral ((acc' `shiftR` 1) `xor` negate (acc' .&. 1)))

getFlag :: Get Bool
getFlag = do
  b <- getWord8
  case b of
    0 -> pure False
    1 -> pure True
    _ -> fail ("bad boolean byte " ++ show b)

getStr :: Strs -> Get Text
getStr tbl = do
  i <- getVar
  case tbl V.!? i of
    Just t -> pure t
    Nothing -> fail ("string index " ++ show i ++ " outside the table of " ++ show (V.length tbl))

getList :: Get a -> Get [a]
getList g = do
  n <- getVar
  when (n < 0) (fail "negative length")
  replicateM n g

getMaybe :: Get a -> Get (Maybe a)
getMaybe g = do
  b <- getWord8
  case b of
    0 -> pure Nothing
    1 -> Just <$> g
    _ -> fail ("bad option byte " ++ show b)

getDraw :: Get TDraw
getDraw = do
  lo <- getVar
  hi <- getVar
  r <- getVar
  unless (lo < hi) (fail "draw with an empty range")
  pure (TDraw lo hi r)

getScalars :: Strs -> Get (Map Text Scalar)
getScalars tbl = M.fromList <$> getList one
  where
    one = do
      k <- getStr tbl
      tag <- getWord8
      case tag of
        0 -> (,) k . SInt <$> getVar
        1 -> (,) k . SText <$> getStr tbl
        _ -> fail ("bad scalar tag " ++ show tag)

getConds :: Strs -> Get (Map Text Cond)
getConds tbl = M.fromList <$> getList one
  where
    one = do
      k <- getStr tbl
      fl <- getWord8
      when (fl > 3) (fail "bad condition flags")
      d <- if fl .&. 1 /= 0 then Just <$> getVar else pure Nothing
      l <- if fl .&. 2 /= 0 then Just <$> getVar else pure Nothing
      pure (k, Cond d l)

getMon :: Strs -> Get MonSnap
getMon tbl = do
  orig <- getVar
  species <- getStr tbl
  level <- getVar
  gender <- getStr tbl
  hp <- getVar
  maxhp <- getVar
  status <- getStr tbl
  ss <- getScalars tbl
  fl <- getWord8
  when (fl > 3) (fail "bad Pokemon flags")
  [a, b, c, d, e, a', b', c', d', e', f', g', spd] <- replicateM 13 getVar
  types <- getList (getStr tbl)
  tera <- getStr tbl
  terad <- getStr tbl
  ability <- getStr tbl
  item <- getStr tbl
  moves <- getList (MoveSnap <$> getStr tbl <*> getVar <*> getVar <*> getFlag)
  vols <- M.fromList <$> getList ((,) <$> getStr tbl <*> getScalars tbl)
  pure (MonSnap orig species level gender hp maxhp status ss (fl .&. 1 /= 0) (fl .&. 2 /= 0) (StatsSnap a b c d e)
    (BoostsSnap a' b' c' d' e' f' g') spd types tera terad ability item moves vols)

getSide :: Strs -> SideSnap -> Get SideSnap
getSide tbl prev = do
  sid <- getStr tbl
  left <- getVar
  conds <- getConds tbl
  mons <- getList one
  pure (SideSnap sid left conds mons)
  where
    one = do
      tag <- getWord8
      case tag of
        0 -> do
          o <- getVar
          maybe (fail ("back-reference to Pokemon " ++ show o ++ ", which the previous snapshot lacks")) pure
            (find ((== o) . mOrig) (ssMons prev))
        1 -> getMon tbl
        _ -> fail ("bad Pokemon tag " ++ show tag)

getSnap :: Strs -> Snap -> Get Snap
getSnap tbl prev = do
  turn <- getVar
  request <- getStr tbl
  ended <- getFlag
  winner <- getMaybe (getStr tbl)
  field <- FieldSnap <$> getStr tbl <*> getMaybe getVar <*> getStr tbl <*> getMaybe getVar <*> getConds tbl
  s1 <- getSide tbl (snP1 prev)
  s2 <- getSide tbl (snP2 prev)
  pure (Snap turn request ended winner field s1 s2)

getBody :: Get Trace
getBody = do
  n <- getVar
  when (n < 0) (fail "negative string table size")
  tbl <- V.fromList <$> replicateM n (getVar >>= \len -> do
    when (len < 0) (fail "negative string length")
    bs <- getByteString len
    either (\e -> fail ("string is not UTF-8: " ++ show e)) pure (TE.decodeUtf8' bs))
  version <- getVar
  sv <- getStr tbl
  fmt <- getStr tbl
  seed <- getStr tbl
  policy <- getVar
  tera <- getWord8 >>= \b -> case b of
    0 -> pure Nothing
    1 -> pure (Just False)
    2 -> pure (Just True)
    _ -> fail ("bad tera byte " ++ show b)
  truncated <- getFlag
  tlen <- getVar
  when (tlen < 0) (fail "negative teams length")
  teamsBytes <- getByteString tlen
  teams <- either (\e -> fail ("teams: " ++ e)) pure (eitherDecodeStrict' teamsBytes :: Either String Value)
  nsteps <- getVar
  when (nsteps < 0) (fail "negative step count")
  let go :: Snap -> Int -> [Step] -> Get ([Step], Snap)
      go prev 0 acc = pure (reverse acc, prev)
      go prev k acc = do
        ds <- getList getDraw
        st <- getSnap tbl prev
        legal <- (,) <$> getList (getStr tbl) <*> getList (getStr tbl)
        chosen <- (,) <$> getMaybe (getStr tbl) <*> getMaybe (getStr tbl)
        go st (k - 1) (Step ds st legal chosen : acc)
  (steps, lastSnap) <- go emptySnap nsteps []
  fdraws <- getList getDraw
  final <- getSnap tbl lastSnap
  pure (Trace version sv fmt seed policy tera truncated teams steps fdraws final)

-- | Decompress a zlib stream; a damaged one is a @Left@ rather than an exception.
inflate :: BL.ByteString -> Either String BL.ByteString
inflate input = ZI.foldDecompressStreamWithInput
  (\c r -> (c :) <$> r)
  (\rest -> if BL.null rest then Right [] else Left "trailing bytes after the compressed body")
  (\e -> Left ("zlib: " ++ show e))
  (ZI.decompressST ZI.zlibFormat ZI.defaultDecompressParams)
  input
  >>= Right . BL.fromChunks

-- | Decode a v2 file. Total: a file that is not a well-formed v2 trace is a @Left@.
decodeV2 :: BL.ByteString -> Either String Trace
decodeV2 file = do
  let (hdr, rest) = BL.splitAt (fromIntegral (B.length magic) + 1) file
  unless (BL.length hdr == fromIntegral (B.length magic) + 1) (Left "not a v2 trace (too short)")
  unless (BL.take 4 hdr == BL.fromStrict magic) (Left "not a v2 trace (bad magic)")
  unless (BL.index hdr 4 == formatVersion) (Left ("unsupported trace format version " ++ show (BL.index hdr 4)))
  raw <- inflate rest
  case runGetOrFail getBody raw of
    Left (_, off, e) -> Left ("corrupt trace at byte " ++ show off ++ ": " ++ e)
    Right (left, off, t)
      | BL.null left -> Right t
      | otherwise -> Left ("corrupt trace: trailing bytes after byte " ++ show off)
