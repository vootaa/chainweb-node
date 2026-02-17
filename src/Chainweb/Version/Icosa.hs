{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Chainweb.Version.Icosa (icosa, pattern Icosa) where

import Chainweb.BlockCreationTime
import qualified Chainweb.BlockHeader.Genesis.Icosa0Payload as I0
import qualified Chainweb.BlockHeader.Genesis.Icosa1to19Payload as I119
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version
import qualified Data.Set as Set
import Pact.Types.Verifier

pattern Icosa :: ChainwebVersion
pattern Icosa <- ((== icosa) -> True)
  where
    Icosa = icosa

icosa :: ChainwebVersion
icosa =
  ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_0012,
      _versionName = ChainwebVersionName "icosa",
      _versionForks = tabulateHashMap $ \case
        _ -> AllChains ForkAtGenesis,
      _versionUpgrades = AllChains mempty,
      _versionGraphs = Bottom (minBound, twentyChainGraph),
      _versionBlockDelay = BlockDelay 30_000_000,
      _versionWindow = WindowWidth 120,
      _versionHeaderBaseSizeBytes = 318 - 110,
      _versionBootstraps = [],
      _versionGenesis =
        VersionGenesis
          { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000),
            _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2019-07-17T18:28:37.613832 |],
            _genesisBlockPayload =
              onChains
                ( (unsafeChainId 0, I0.payloadBlock)
                    : [(unsafeChainId i, I119.payloadBlock) | i <- [1 .. 19]]
                )
          },
      _versionMaxBlockGasLimit = Bottom (minBound, Nothing),
      _versionMinimumBlockHeaderHistory = Bottom (minBound, Nothing),
      _versionCheats =
        VersionCheats
          { _disablePow = True,
            _fakeFirstEpochStart = False,
            _disablePact = False
          },
      _versionDefaults =
        VersionDefaults
          { _disablePeerValidation = False,
            _disableMempoolSync = False
          },
      _versionVerifierPluginNames =
        AllChains $
          Bottom
            (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow", "signed_list"]),
      _versionQuirks = noQuirks,
      _versionForkNumber = 0
    }
