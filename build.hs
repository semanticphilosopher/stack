{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

--FIXME: should we have stack build tool, or should we run it with 'stack runghc build.hs'

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List
import Data.Maybe
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.System
import Distribution.Package
import Distribution.PackageDescription hiding (options)
import Distribution.Verbosity
import System.Console.GetOpt
import System.Directory
import System.IO.Error
import System.Process

import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import Data.Aeson
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Data.List.Extra
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime

main :: IO ()
main = do
    gPkgDescr <- readPackageDescription silent "stack.cabal"
    instRoot <- readProcess "stack" ["path", "--local-install-root"] ""
    let gLocalInstallRoot = trim (fromMaybe instRoot (stripPrefix "local-install-root:" instRoot))
        gGpgKey = Nothing
        gAllowDirty = False
        gGithubAuthToken = Nothing
        gGithubReleaseTag = Nothing
    shakeArgsWith
        shakeOptions{ shakeFiles = buildDir
                    , shakeVerbosity = Chatty
                    , shakeChange = ChangeModtimeAndDigestInput
                    , shakeLint = Just LintBasic }
        options
        (\flags args -> return $ Just $ rules (foldl (flip id) Global{..} flags) args)

options :: [OptDescr (Either String (Global -> Global))]
options =
    [ Option "" ["gpg-key"]
        (ReqArg (\v -> Right $ \g -> g{gGpgKey = Just v}) "USER-ID")
        --FIXME: make clear that this key is not used for Ubuntu packages
        "GPG user ID to sign distribution package with"
    , Option "" ["allow-dirty"] (NoArg $ Right $ \g -> g{gAllowDirty = True})
        "Allow a dirty working tree for release."
    , Option "" ["github-auth-token"]
        (ReqArg (\v -> Right $ \g -> g{gGithubAuthToken = Just v}) "TOKEN")
        "Github personal access token."
    , Option "" ["github-release-tag"]
        (ReqArg (\v -> Right $ \g -> g{gGithubReleaseTag = Just v}) "TAG")
        "Github release tag to upload to." ]

rules :: Global -> [String] -> Rules ()
rules global@Global{..} args = do
    case args of
        [] -> want [buildPhony]
        _ -> want args

    phony cleanPhony $ do
        () <- cmd "stack clean"
        removeFilesAfter buildDir ["//*"]

    phony buildPhony $
        need [buildDir </> stackExeFileName]

    --FIXME: add .deb rules

    phony releasePhony $ do
        need [releaseCheckPhony]
        need [releaseUploadPhony]

    phony releaseCleanPhony $ 
        removeFilesAfter releaseDir ["//*"]

    phony releaseCheckPhony $
        need [releaseCheckDir </> stackExeFileName]

    phony releaseUploadPhony $
        need $ map (releaseUploadDir </>) releaseFileNames

    phony releaseBuildPhony $
        need $ map (releaseDir </>) releaseFileNames

    releaseUploadDir </> "*" %> \out -> do
        need [releaseDir </> takeFileName out]
        uploadGithubRelease global (releaseDir </> takeFileName out)
        copyFile' (releaseDir </> takeFileName out) out
        
    releaseCheckDir </> stackExeFileName %> \out -> do
        need [buildDir </> stackExeFileName]
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error "Working tree is dirty.  Use --allow-dirty option to continue anyway."
        let instExeFile = installBinDir </> stackExeFileName
            tmpExeFile = installBinDir </> stackExeFileName <.> "tmp"
        --FIXME: once 'stack install --path' implemented, use it instead of this temp file.
        liftIO $ renameFile instExeFile tmpExeFile
        actionFinally
            (do opt <- addPath [installBinDir] []
                () <- cmd opt "stack build"
                () <- cmd opt "stack clean"
                () <- cmd opt "stack build --pedantic"
                () <- cmd opt "stack test --flag stack:integration-tests"
                return ())
            (renameFile tmpExeFile instExeFile)
        copyFile' (buildDir </> stackExeFileName) out

    buildDir </> stackExeFileName %> \out -> do
        alwaysRerun
        () <- cmd "stack build"
        copyFileChanged (installBinDir </> stackExeFileName) out

    --FIXME: will need different rule on Windows to make a .zip
    case platformOS of
        Windows ->
            releaseDir </> releaseExeZipFileName %> \out -> do
                need [buildDir </> stackExeFileName]
                liftIO $ do
                    --XXX TEST (ensure file in root)
                    archive <-
                        Zip.addFilesToArchive [] Zip.emptyArchive [buildDir </> stackExeFileName]
                    L8.writeFile out (Zip.fromArchive archive)
        _ ->
            releaseDir </> releaseExeGzFileName %> \out -> do
                need [buildDir </> stackExeFileName]
                withTempDir $ \tmp -> do
                    copyFile' (buildDir </> stackExeFileName) (tmp </> stackExeFileName)
                    () <- cmd "strip" [tmp </> stackExeFileName]
                    liftIO $ do
                        fc <- L8.readFile (tmp </> stackExeFileName)
                        L8.writeFile out $ GZip.compress fc

    releaseDir </> releaseExeCompressedAscFileName %> \out -> do
        let gpgKey = fromMaybe (error "GPG key required.  Use --gpg-key option to specify.") gGpgKey
        need [out -<.> ""]
        _ <- liftIO $ tryJust (guard . isDoesNotExistError) (removeFile out)
        cmd "gpg --detach-sig --armor -u" [gpgKey] [out -<.> ""]

  where
    buildPhony = "build"
    cleanPhony = "clean"
    releasePhony = "release"
    releaseCheckPhony = "release-check"
    releaseUploadPhony = "release-upload"
    releaseCleanPhony = "release-clean"
    releaseBuildPhony = "release-build"

    releaseCheckDir = releaseDir </> "check"
    releaseUploadDir = releaseDir </> "upload"
    releaseDir = buildDir </> "release"
    installBinDir = gLocalInstallRoot </> "bin"

    stackExeFileName = "stack" <.> exe
    releaseFileNames = [releaseExeCompressedFileName, releaseExeCompressedAscFileName]
    releaseExeCompressedAscFileName = releaseExeCompressedFileName <.> ascExt
    releaseExeCompressedFileName =
        case platformOS of
            Windows -> releaseExeZipFileName
            _ -> releaseExeGzFileName
    releaseExeZipFileName = releaseExeFileName -<.> zipExt
    releaseExeGzFileName = releaseExeFileName <.> gzExt
    releaseExeFileName = releaseName global <.> exe

    zipExt = "zip"
    gzExt = "gz"
    ascExt = "asc"

uploadGithubRelease :: Global -> FilePath -> Action ()
uploadGithubRelease global@Global{..} file = do
    GithubRelease{..} <- getGithubRelease
    resp <- liftIO $ callGithubApi global
        [(CI.mk $ S8.pack "Content-Type", defaultMimeLookup (T.pack file))]
        (Just file)
        (replace
            "{?name}"
            ("?name=" ++ S8.unpack (urlEncode True (S8.pack (takeFileName file))))
            relUploadUrl)
    case eitherDecode resp of
        Left e -> error ("Could not parse Github asset upload response (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
        Right (GithubReleaseAsset{..}) ->
            when (assetState /= "uploaded") $
                error ("Invalid asset state after Github asset upload: " ++ assetState)
  where

    getGithubRelease = do
        releases <- getGithubReleases
        let tag = fromMaybe ("v" ++ stackVersionStr global) gGithubReleaseTag
        return $ fromMaybe 
            (error ("Could not find Github release with tag '" ++ tag ++ "'.\n" ++
                    "Use --github-release-tag option to specify a different tag."))
            (find (\r -> relTagName r == tag) releases)

    getGithubReleases = do
        resp <- liftIO $ callGithubApi global
            [] Nothing "https://api.github.com/repos/commercialhaskell/stack/releases"
        case eitherDecode resp of
            Left e -> error ("Could not parse Github releases (" ++ e ++ "):\n" ++ L8.unpack resp ++ "\n")
            Right r -> return r

callGithubApi :: Global -> RequestHeaders -> Maybe FilePath -> String -> IO L8.ByteString
callGithubApi Global{..} headers mpostFile url = do
    req0 <- parseUrl url
    let authToken =
            fromMaybe (error "Github auth token required.  Use --github-auth-token option to specify.") gGithubAuthToken
        req1 =
            req0
                { checkStatus = \_ _ _ -> Nothing
                , requestHeaders =
                    [ (CI.mk $ S8.pack "Authorization", S8.pack $ "token " ++ authToken)
                    , (CI.mk $ S8.pack "User-Agent", S8.pack "commercialhaskell/stack") ] ++
                    headers }
    req <- case mpostFile of
        Nothing -> return req1
        Just postFile -> do
            lbs <- L8.readFile postFile
            return $ req1
                { method = S8.pack "POST"
                , requestBody = RequestBodyLBS lbs }
    withManager $ \manager -> do
        res <- http req manager
        responseBody res $$+- CC.sinkLazy
            
releaseName :: Global -> String
releaseName global = "stack-" ++ stackVersionStr global ++ "-" ++ platformName

stackVersionStr :: Global -> String
stackVersionStr = display . pkgVersion . package . packageDescription . gPkgDescr

platformName :: String
--FIXME: does this produce desired result in all cases?
platformName = display buildPlatform

platformOS :: OS
platformOS =
    let Platform _ os = buildPlatform
    in os

buildDir :: FilePath
buildDir = "_build"

data GithubRelease = GithubRelease
    { relUploadUrl :: !String
    , relTagName :: !String }
    deriving (Show)

instance FromJSON GithubRelease where
    parseJSON = withObject "GithubRelease" $ \o ->
        GithubRelease
        <$> o .: T.pack "upload_url"
        <*> o .: T.pack "tag_name"

data GithubReleaseAsset = GithubReleaseAsset
    { assetState :: !String }
    deriving (Show)

instance FromJSON GithubReleaseAsset where
    parseJSON = withObject "GithubReleaseAsset" $ \o ->
        GithubReleaseAsset
        <$> o .: T.pack "state"

data Global = Global
    { gPkgDescr :: !GenericPackageDescription
    , gLocalInstallRoot :: !FilePath
    , gGpgKey :: !(Maybe String)
    , gAllowDirty :: !Bool
    , gGithubAuthToken :: !(Maybe String)
    , gGithubReleaseTag :: !(Maybe String)
    }
