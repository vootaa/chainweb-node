{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Chainweb.Version.TriadDev (triadDev, pattern TriadDev) where

import Control.Lens

import Chainweb.Version
import Chainweb.Version.Triad

pattern TriadDev :: ChainwebVersion
pattern TriadDev <- ((== triadDev) -> True) where
    TriadDev = triadDev

triadDev :: ChainwebVersion
triadDev = triad
    & versionCode .~ ChainwebVersionCode 0x00000021
    & versionName .~ ChainwebVersionName "triad-dev"
    & versionCheats . disablePow .~ True
    & versionCheats . fakeFirstEpochStart .~ True
    & versionDefaults . disablePeerValidation .~ True
