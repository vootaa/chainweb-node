{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Mono(mono, pattern Mono) where

import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import Pact.Types.Verifier

import qualified Chainweb.BlockHeader.Genesis.Mono0Payload as M0

pattern Mono :: ChainwebVersion
pattern Mono <- ((== mono) -> True) where
    Mono = mono

mono :: ChainwebVersion
mono = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_0010
    , _versionName = ChainwebVersionName "mono"
    , _versionForks = tabulateHashMap $ \case
        _ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = Bottom (minBound, singletonChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, M0.payloadBlock)
            ]
        }

    , _versionMaxBlockGasLimit = Bottom (minBound, Nothing)
    , _versionMinimumBlockHeaderHistory = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = False
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = False
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = AllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow", "signed_list"])
    , _versionQuirks = noQuirks
    , _versionForkNumber = 0
    }
