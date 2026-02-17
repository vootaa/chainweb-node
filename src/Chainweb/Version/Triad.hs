{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

module Chainweb.Version.Triad(triad, pattern Triad) where

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

import qualified Chainweb.BlockHeader.Genesis.Triad0Payload as T0
import qualified Chainweb.BlockHeader.Genesis.Triad1to2Payload as T12

pattern Triad :: ChainwebVersion
pattern Triad <- ((== triad) -> True) where
    Triad = triad

triad :: ChainwebVersion
triad = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_0011
    , _versionName = ChainwebVersionName "triad"
    , _versionForks = tabulateHashMap $ \case
        _ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = Bottom (minBound, triangleChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |]
        , _genesisBlockPayload = onChains
            [ (unsafeChainId 0, T0.payloadBlock)
            , (unsafeChainId 1, T12.payloadBlock)
            , (unsafeChainId 2, T12.payloadBlock)
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
