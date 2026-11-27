{-# LANGUAGE OverloadedStrings #-}

-- | Reports how much of the usage whitelist (data/usage/gen9ou.json, from
-- tools/usage/gen-whitelist.js) the engine supports, as Markdown on stdout.
-- Support is whatever 'supportedSpecies', 'supportedMoves', 'supportedAbilities' and 'supportedItems' say. The
-- engine terastallizes (slice 5) to every type, Stellar included, so every tera type is supported.
--
-- Usage: whitelist-report [usageFile=data/usage/gen9ou.json] [dexDir=data/gen9]
module Main (main) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Scientific (toRealFloat)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Engine.Dex (Dex (..), loadDex, supportedAbilities, supportedItems, supportedMoves, supportedSpecies)
import System.Environment (getArgs)
import Text.Printf (printf)

type Weights = [(T.Text, Double)]

main :: IO ()
main = do
  args <- getArgs
  let usageFile = case args of (f : _) -> f; _ -> "data/usage/gen9ou.json"
      dexDir = case args of (_ : d : _) -> d; _ -> "data/gen9"
  dex <- loadDex dexDir
  usage <- load usageFile
  let weights k = case field k usage of
        Just (Object o) -> [(K.toText i, num v) | (i, v) <- KM.toList o]
        _ -> []
      speciesW = case field "species" usage of
        Just (Object o) -> [(K.toText i, maybe 0 num (field "usage" v)) | (i, v) <- KM.toList o]
        _ -> []
      supported =
        [ ("species", Set.fromList (supportedSpecies dex))
        , ("moves", Set.fromList (supportedMoves dex))
        , ("abilities", Set.fromList (supportedAbilities dex))
        , ("items", Set.fromList (supportedItems dex))
        , ("teraTypes", Set.fromList (map T.toLower (V.toList (dxTypeNames dex)) ++ ["stellar"]))
        ] :: [(T.Text, Set.Set T.Text)]
      cats =
        [ ("species", speciesW), ("moves", weights "moves"), ("abilities", weights "abilities")
        , ("items", weights "items"), ("teraTypes", weights "teraTypes") ]
  handlers <- M.fromList <$> traverse (\f -> (,) f <$> loadHandlers dexDir f)
    ["moves", "abilities", "items"]
  TIO.putStrLn "# Whitelist coverage\n"
  TIO.putStrLn "| category | supported/total | by weight |"
  TIO.putStrLn "|---|---|---|"
  mapM_ (summary supported) cats
  mapM_ (detail supported handlers) cats
  where
    summary supported (cat, ws) = do
      let sup = maybe Set.empty id (lookup cat supported)
          (yes, _) = split sup ws
          wsum = sum . map snd
          pct = if wsum ws > 0 then 100 * wsum yes / wsum ws else 0 :: Double
      printf "| %s | %d/%d | %.1f%% |\n" (T.unpack cat) (length yes) (length ws) pct

    detail supported handlers (cat, ws) = do
      let sup = maybe Set.empty id (lookup cat supported)
          (_, no) = split sup ws
          hs = M.findWithDefault M.empty cat handlers
      TIO.putStrLn ("\n## Unsupported " <> cat <> " (" <> T.pack (show (length no)) <> ")\n")
      mapM_
        ( \(i, w) -> do
            let h = M.findWithDefault [] i hs
            TIO.putStrLn
              ( "- " <> i <> " " <> T.pack (printf "%.4f" w)
                  <> (if null h then "" else " [" <> T.intercalate ", " h <> "]")
              )
        )
        (sortBy (comparing (Down . snd) <> comparing fst) no)

split :: Set.Set T.Text -> Weights -> (Weights, Weights)
split sup = foldr (\x@(i, _) (y, n) -> if i `Set.member` sup then (x : y, n) else (y, x : n)) ([], [])

num :: Value -> Double
num (Number n) = toRealFloat n
num _ = 0

field :: T.Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

load :: FilePath -> IO Value
load f = eitherDecodeFileStrict f >>= either (ioError . userError . ((f ++ ": ") ++)) pure

-- | Showdown handler names per id, from the @handlers@ field of data/gen9/<name>.json.
loadHandlers :: FilePath -> T.Text -> IO (M.Map T.Text [T.Text])
loadHandlers dir name = do
  v <- load (dir ++ "/" ++ T.unpack name ++ ".json")
  pure $ case v of
    Object o ->
      M.fromList
        [ (K.toText i, [t | String t <- V.toList hs])
        | (i, e) <- KM.toList o
        , Just (Array hs) <- [field "handlers" e]
        ]
    _ -> M.empty
