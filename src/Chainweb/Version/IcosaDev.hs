{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Chainweb.Version.IcosaDev (icosaDev, pattern IcosaDev) where

import Control.Lens

import Chainweb.Version
import Chainweb.Version.Icosa

pattern IcosaDev :: ChainwebVersion
pattern IcosaDev <- ((== icosaDev) -> True) where
    IcosaDev = icosaDev

icosaDev :: ChainwebVersion
icosaDev = icosa
    & versionCode .~ ChainwebVersionCode 0x00000022
    & versionName .~ ChainwebVersionName "icosa-dev"
    & versionCheats . disablePow .~ True
    & versionCheats . fakeFirstEpochStart .~ True
    & versionDefaults . disablePeerValidation .~ True
