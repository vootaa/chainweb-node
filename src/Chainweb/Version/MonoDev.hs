{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Chainweb.Version.MonoDev (monoDev, pattern MonoDev) where

import Control.Lens

import Chainweb.Version
import Chainweb.Version.Mono

pattern MonoDev :: ChainwebVersion
pattern MonoDev <- ((== monoDev) -> True) where
    MonoDev = monoDev

monoDev :: ChainwebVersion
monoDev = mono
    & versionCode .~ ChainwebVersionCode 0x00000020
    & versionName .~ ChainwebVersionName "mono-dev"
    & versionCheats . disablePow .~ True
    & versionCheats . fakeFirstEpochStart .~ True
    & versionDefaults . disablePeerValidation .~ True
