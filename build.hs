{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

--FIXME: should we have stack build tool, or should we run it with 'stack runghc build.hs'

import Control.Monad
import Data.Char
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
import System.Process

import Development.Shake
import Development.Shake.FilePath

main :: IO ()
main = do
    gPkgDescr <- readPackageDescription silent "stack.cabal"
    instRoot <- readProcess "stack" ["path", "--local-install-root"] ""
    let gLocalInstallRoot = trim (fromMaybe instRoot (stripPrefix "local-install-root:" instRoot))
        gGpgKey = Nothing
        gAllowDirty = False
    shakeArgsWith
        shakeOptions{ shakeFiles = "_build"
                    , shakeVerbosity = Chatty
                    , shakeChange = ChangeModtimeAndDigestInput
                    , shakeLint = Just LintBasic }
        options
        (\flags args -> return $ Just $ rules (foldl (flip id) Global{..} flags) args)

options :: [OptDescr (Either String (Global -> Global))]
options =
    [ Option "" ["gpg-key"]
        (ReqArg (\k -> Right $ \g -> g{gGpgKey = Just k}) "USER-ID")
        --FIXME: make clear that this key is not used for Ubuntu packages
        "GPG user ID to sign distribution package with"
    , Option "" ["allow-dirty"] (NoArg $ Right $ \c -> c{gAllowDirty = True})
        "Allow a dirty working tree for release." ]

rules :: Global -> [String] -> Rules ()
rules global@Global{..} args = do
    case args of
        [] -> want ["build"]
        _ -> want args

    phony "clean" $ do
        putNormal "Cleaning"
        () <- cmd "stack clean"
        removeFilesAfter "_build" ["//*"]

    phony "release" $ do
        need ["pre-release"]
        need ["build-release"]

    --FIXME: add "upload-release" rule
    --FIXME: add .deb rules

    phony "pre-release" $ do
        Stdout dirty <- cmd "git status --porcelain"
        when (not gAllowDirty && not (null (trim dirty))) $
            error "Working tree is dirty.  Use --allow-dirty option to continue anyway."
        () <- cmd "stack build"
        opt <- addPath [gLocalInstallRoot </> "bin"] []
        () <- cmd opt "stack clean"
        () <- cmd opt "stack build --pedantic"
        () <- cmd opt "stack test --flag stack:integration-tests"
        return ()

    phony "build-release" $
        need
            [ "_build/release" </> distName global <.> exe <.> "gz.asc"
            , "_build/release" </> distName global <.> exe <.> "gz" ]

    phony "build" $
        need ["_build/stack" <.> exe]

    "_build/stack" <.> exe %> \out -> do
        alwaysRerun
        () <- cmd "stack build"
        copyFileChanged (gLocalInstallRoot </> "bin/stack") out

    --FIXME: will need different rule on Windows to make a .zip
    "_build/release" </> distName global <.> exe <.> "gz" %> \out ->
        withTempDir $ \tmp -> do
            copyFile' ("_build/stack" <.> exe) (tmp </> "stack" <.> exe)
            () <- cmd "strip" [tmp </> "stack" <.> exe]
            () <- cmd "gzip" [tmp </> "stack" <.> exe]
            liftIO $ copyFile (tmp </> "stack" <.> exe <.> "gz") out

    "_build/release" </> distName global <.> exe <.> "gz.asc" %> \out -> do
        let gpgKey = fromMaybe (error "GPG key required.  Use --gpg-key option to specify.") gGpgKey
        need [out -<.> ""]
        liftIO $ removeFile out
        cmd "gpg --detach-sig --armor -u" [gpgKey] [out -<.> ""]
            
distName :: Global -> String
distName global = "stack-" ++ stackVersionStr global ++ "-" ++ platformStr

stackVersionStr :: Global -> String
stackVersionStr = display . pkgVersion . package . packageDescription . gPkgDescr

platformStr :: String
--FIXME: does this produce desired result in all cases?
platformStr = display buildPlatform

--FIXME: could use this from 'extra' package
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

data Global = Global
    { gPkgDescr :: !GenericPackageDescription
    , gLocalInstallRoot :: !FilePath
    , gGpgKey :: !(Maybe String)
    , gAllowDirty :: !Bool
    }
