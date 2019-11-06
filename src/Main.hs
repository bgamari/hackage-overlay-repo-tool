{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup            hiding (option)
import qualified Data.Set                  as Set
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FP
import           Shelly
import           System.IO
import           Options.Applicative

default (T.Text)

data Config
  = Config
  { _patches           :: FP.FilePath -- ^ path to the @patches@ folder. Should contain @<package-id>-<package-version>.patch@ files.
  , _keys              :: FP.FilePath -- ^ path to the keys, as generated with the @hackage-repo-tool@.
  , _template          :: FP.FilePath -- ^ template repo. This will be copied into the temporary repo, and can contain additional files as needed.
  , _remote_repo_cache :: FP.FilePath -- ^ path to the package cache
  , _remote_repo_name  :: String   -- ^ name of the remote repo
  , _remote_repo_url   :: String   -- ^ url of the remote repo
  --
  , _tar_cmd           :: FP.FilePath -- ^ name of the @tar@ command.
  , _target            :: Text   -- ^ e.g. user@host:/path/to/repo/
  }
  deriving (Show)

configParser :: Parser Config
configParser = Config
               <$> strOption (value "patches" <> showDefault <> long "patches"  <> metavar "PATCHES"  <> help "Folder containing the patches")
               <*> strOption (value ".keys"   <> showDefault <> long "keys"     <> metavar "KEYS"     <> help "Folder containing the repo-tool keys")
               <*> strOption (value ".tmpl"   <> showDefault <> long "template" <> metavar "TEMPLATE" <> help "Template repository to use as a starting point")
               <*> strOption (value "packages" <> showDefault <> long "repo-cache" <> metavar "REPOCACHE" <> help "The path to the package cache.")
               <*> strOption (value "hackage.haskell.org" <> showDefault <> long "repo-name" <> metavar "REPONAME" <> help "The name of the remote repo.")
               <*> strOption (value "http://hackage-origin.haskell.org/" <> showDefault <> long "repo-url" <> metavar "URL" <> help "The url of the remote repo.")
               <*> strOption (value "tar" <> showDefault <> long "tar" <> metavar "TAR" <> help "`tar` command.")
               <*> argument str (metavar "TARGET" <> help "The rsync target e.g. user@host:/path/to/repo; or an s3 bucket e.g. s3://<bucket>/ -- will use `aws` cli")

main :: IO ()
main = hSetBuffering stdout LineBuffering
       >> execParser opts
       >>= shelly . verbosely . mkOverlay
  where opts = info (configParser <**> helper)
          (fullDesc
           <> progDesc "Hackage overlay generator"
           <> header "tool - a tool for generating hackage overlays")

-- | We can't fetch all packages at once due to
-- https://gitlab.haskell.org/ghc/head.hackage/issues/6, wherein @cabal fetch@
-- will only fetch one version of each requested package per invocation.
--
-- We used to just fetch each package individually but this was quite slow. To
-- speed things up a bit we break up the set of packages to be fetched into
-- batches, each containing at most one version of each package name.
batchFetchSources :: Set.Set PkgId -> [Set.Set PkgId]
batchFetchSources pkgs =
    let pkgVersions0 :: Map.Map Text (Set.Set PkgId)
        pkgVersions0 = Map.fromListWith (<>)
          [ (pn, Set.singleton pkgid)
          | pkgid@(PkgId pn pv) <- Set.toList pkgs
          ]

        go :: Map.Map Text (Set.Set PkgId) -> [Set.Set PkgId]
        go pkgVersions
          | Map.null pkgVersions = []
          | otherwise = thisBatch : go pkgVersions'
          where
            thisBatch :: Set.Set PkgId
            thisBatch = foldMap (Set.singleton . fst . minView) pkgVersions

            pkgVersions' = dropEmpties $ fmap (snd . minView) pkgVersions

            dropEmpties :: Map.Map k (Set.Set a) -> Map.Map k (Set.Set a)
            dropEmpties = Map.filter (not . Set.null)

            minView :: Set.Set a -> (a, Set.Set a)
            minView = fromMaybe (error "empty set") . Set.minView
    in go pkgVersions0

fetchSources :: Config -> Set.Set PkgId -> Sh ()
fetchSources config pkgs = do
  withTmpDir $ \tmpdir -> do
    let cfgFile = tmpdir </> "cabal.cfg"
    writefile cfgFile $ T.unlines
      [ "repository " <> toTextArg (_remote_repo_name config)
      , "  url: " <> toTextArg (_remote_repo_url config)
      , "  secure: True"
      , ""
      , "http-transport: plain-http"
      , "remote-repo-cache: " <> toTextIgnore (_remote_repo_cache config)
      ]
    run_ "cabal"  ["--config-file=" <> toTextIgnore cfgFile, "update"]

    let batches = batchFetchSources pkgs
    let fetch pkg =
          run_ "cabal" $ ["--config-file=" <> toTextIgnore cfgFile, "fetch", "--no-dependencies"] ++ map pid2txt (Set.toList pkg)
    mapM_ fetch batches
    --run_ "cabal" (["--config-file=" <> toTextIgnore cfgFile, "fetch", "--no-dependencies"] ++
    --              map pid2txt (Set.toList pkgs))

mkOverlay :: Config -> Sh ()
mkOverlay config = do
  unlessM (test_d (_patches config)) $
      errorExit "patches folder not found"

  unlessM (test_d (_keys config)) $
      errorExit "keys folder not found"

  rm_rf "repo.tmp"
  mkdir "repo.tmp"
  mkdir "repo.tmp/package"
  mkdir "repo.tmp/index"

  mkdir_p "patches.cache"

  tmpl_files <- ls (_template config)
  forM_ tmpl_files $ \path ->
    cp_r path "repo.tmp/"

  pkgDir   <- absPath "repo.tmp/package"
  idxDir   <- absPath "repo.tmp/index"
  patchDir <- absPath (_patches config)
  patchCacheDir <- absPath $ (_patches config) <.> "cache"

  pfns <- ls (_patches config)

  let cabalFns0 = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "cabal") pfns
      patchFns  = Set.fromList $ map (fn2pid . FP.filename) $ filter (hasExt "patch") pfns

      -- .cabal only fixups via revisions
      cabalFns = cabalFns0 Set.\\ patchFns

  -- pre-fetch packages
  fetchSources config (cabalFns <> patchFns)

  let get_pkgcache :: PkgId -> Sh FP.FilePath
      get_pkgcache (PkgId pn pv) = absPath $ (_remote_repo_cache config) </> (_remote_repo_name config) </> pn </> pv </> (pn <> "-" <> pv) <.> "tar.gz"

  forM_ patchFns $ \pid@(PkgId pn pv) -> do
      pkg <- get_pkgcache pid
      withTmpDir $ \tmpdir -> do
          let p       = pid2txt pid
              patchFn = patchDir </> (p <.> "patch")

              patchCacheFn   = patchCacheDir </> (p <.> "patch")
              tarOrigCacheFn = patchCacheDir </> (p <.> "tar.gz.orig")
              tarCacheFn     = patchCacheDir </> (p <.> "tar.gz")

          cacheHitP <- isSameContent patchFn patchCacheFn
          cacheHitT <- isSameContent pkg tarOrigCacheFn
          let cacheHit = cacheHitT && cacheHitP

          if not cacheHit
            then -- cache MISS
              chdir tmpdir $ do
                  run_ (_tar_cmd config) [ "-xf", toTextIgnore pkg ]

                  chdir (fromText p) $ do
                      unlessM (test_f (pn <.> "cabal")) $
                          errorExit "cabal file not found"

                      unlessM (test_f patchFn) $
                          errorExit ("patch file not found " <> T.pack (show patchFn))

                      run_ "patch" ["-i", toTextIgnore patchFn, "-p1", "--no-backup-if-mismatch"]

                  run_ (_tar_cmd config)
                    [ "-cz", "--format=ustar"
                    , "--numeric-owner", "--owner=root", "--group=root"
                    , "--clamp-mtime", "--mtime=" <> T.pack (FP.encodeString patchFn)
                    , "-f", p <> ".tar.gz"
                    , p <> "/"
                    ]

                  cp ("." </> p <.> "tar.gz") pkgDir

                  -- update cache
                  cp patchFn                  patchCacheFn
                  cp pkg                      tarOrigCacheFn
                  cp ("." </> p <.> "tar.gz") tarCacheFn
            else -- cache HIT
              cp tarCacheFn pkgDir

  forM_ cabalFns $ \pid@(PkgId pn pv) -> do
      pkg <- get_pkgcache pid
      cp pkg pkgDir

  run_ "hackage-repo-tool" ["bootstrap", "--keys", toTextIgnore (_keys config), "--repo", "repo.tmp/", "--verbose"]

  sleep 2

  forM_ cabalFns0 $ \pid@(PkgId pn pv) -> do
      withTmpDir $ \tmpdir -> do
          chdir tmpdir $ do
              let p = pid2txt pid
                  cabalFn = patchDir </> (p <.> "cabal")

              cp cabalFn (idxDir </> pn </> pv </> (pn <.> "cabal"))

  run_ "hackage-repo-tool" ["update", "--keys", toTextIgnore (_keys config), "--repo", "repo.tmp/", "--verbose"]

  rm_f "repo.tmp/01-index.tar"
  rm_rf "repo.tmp/index"

  if "s3://" `T.isPrefixOf` (_target config)
    then run_ "aws" ["s3", "sync", "repo.tmp/", (_target config)]
    else run_ "rsync" ["--delete", "-cvrz", "-e", "ssh", "repo.tmp/", (_target config)]

  return ()

  where
    isSameContent :: Shelly.FilePath -> Shelly.FilePath -> Sh Bool
    isSameContent ref subj = do
      ex <- test_f subj
      if ex
        then (==) <$> readBinary ref <*> readBinary subj
        else pure False

data PkgId = PkgId !Text !Text
           deriving (Show,Eq,Ord)

pid2txt :: PkgId -> Text
pid2txt (PkgId pn pv) = pn <> "-" <> pv

fn2pid :: FP.FilePath -> PkgId
fn2pid fn = PkgId (T.init pn) pv
  where
    (pn,pv) = T.breakOnEnd "-" t

    t = case toTextIgnore fn of
          t' | Just t'' <- T.stripSuffix ".patch" t' -> t''
             | Just t'' <- T.stripSuffix ".cabal" t' -> t''
