{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Ea.Genesis
( -- * Genesis tx data
  Genesis(..)
, ChainIdRange
, pattern ChainIdRange
, mkChainIdRange
, onlyChainId
, chainIdRangeTag

  -- * Devnet Genesis Txs
, recapDevelopment0
, recapDevelopmentN
, recapDevelopmentKAD
, fastDevelopment0
, fastDevelopmentN

  -- * Vootaa Genesis txs
, mono0
, triad0
, triadN
, icosa0
, icosaN

  -- * Testing Genesis Txs
, fastTimedCPM0
, fastTimedCPMN
, instantCPM0
, instantCPMN
, pact5InstantCPM0
, pact5InstantCPMN
, pact53TransitionCPM0
, pact53TransitionCPMN
, quirkedPact5InstantCPM0
, quirkedPact5InstantCPMN

  -- * Coin Contract genesis
, coinContractV1
, coinContractV2
, coinContractV2Install
, coinContractV3
, coinContractV4
, coinContractV5
, coinContractV6
, fungibleAssetV1
, fungibleAssetV2
, fungibleXChainV1
, gasPayer
) where


import Control.Lens
import Control.Monad

import Data.Text qualified as T
import Data.Word

import Chainweb.Graph
import Chainweb.Test.TestVersions
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.Icosa
import Chainweb.Version.Mono
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Triad

-- ---------------------------------------------------------------------- --
-- Genesis Tx Data

-- A range of chain IDs [l,r]
data ChainIdRange
    = UnsafeChainIdRange !Word32 !Word32
    deriving (Eq, Ord)

{-# COMPLETE ChainIdRange #-}
pattern ChainIdRange :: Word32 -> Word32 -> ChainIdRange
pattern ChainIdRange l r <- UnsafeChainIdRange l r

mkChainIdRange :: Word32 -> Word32 -> ChainIdRange
mkChainIdRange l u
  | l <= u = UnsafeChainIdRange l u
  | otherwise = error "mkChainIdRange: chain IDs are not in order"

onlyChainId :: Word32 -> ChainIdRange
onlyChainId = join mkChainIdRange

chainIdRangeTag :: ChainIdRange -> String
chainIdRangeTag (ChainIdRange l u)
    | l == u = show l
    | otherwise = show l <> "to" <> show u

-- | Genesis transaction record
--
data Genesis = Genesis
    { _version :: ChainwebVersion
  -- ^ chainweb version
    , _tag :: T.Text
      -- ^ Module name tag
    , _txChainIds :: ChainIdRange
      -- ^ chain id
    , _coinbase :: Maybe FilePath
      -- ^ filepath to coinbase yaml
    , _keysets :: Maybe FilePath
      -- ^ filepath to keyset yaml
    , _allocations :: Maybe FilePath
      -- ^ filepath to allocation yaml
    , _namespaces :: Maybe FilePath
      -- ^ filepath to namespace yaml
    , _coinContract :: [FilePath]
    } deriving (Eq, Ord)

makeLensesFor [("_" <> fn, fn) | fn <- ["txChainIds", "coinbase", "keysets", "allocations", "namespaces", "coinContract"]] ''Genesis

-- ---------------------------------------------------------------------- --
--  Coin Contract Essentials

coinContractV1 :: FilePath
coinContractV1 = "pact/coin-contract/v1/load-coin-contract.yaml"

fungibleAssetV1 :: FilePath
fungibleAssetV1 = "pact/coin-contract/v1/load-fungible-asset.yaml"

gasPayer :: FilePath
gasPayer = "pact/gas-payer/load-gas-payer.yaml"

coinContractV2 :: FilePath
coinContractV2 = "pact/coin-contract/v2/load-coin-contract-v2.yaml"

coinContractV2Install :: FilePath
coinContractV2Install = "pact/coin-contract/v2/install-coin-contract-v2.yaml"

coinContractV3 :: FilePath
coinContractV3 = "pact/coin-contract/v3/load-coin-contract-v3.yaml"

coinContractV4 :: FilePath
coinContractV4 = "pact/coin-contract/v4/load-coin-contract-v4.yaml"

coinContractV5 :: FilePath
coinContractV5 = "pact/coin-contract/v5/load-coin-contract-v5.yaml"

coinContractV6 :: FilePath
coinContractV6 = "pact/coin-contract/load-coin-contract.yaml"

installCoinContractV6 :: FilePath
installCoinContractV6 = "pact/coin-contract/install-coin-contract.yaml"

installCoinContractV6Mono :: FilePath
installCoinContractV6Mono = "pact/coin-contract/install-coin-contract-mono.yaml"

fungibleAssetV2 :: FilePath
fungibleAssetV2 = "pact/coin-contract/v2/load-fungible-asset-v2.yaml"

fungibleXChainV1 :: FilePath
fungibleXChainV1 = "pact/coin-contract/v4/load-fungible-xchain-v1.yaml"

-- ---------------------------------------------------------------------- --
-- Devnet - RecapDevelopment

recapDevelopment0 :: Genesis
recapDevelopment0 = Genesis
    { _version = RecapDevelopment
    , _tag = "RecapDevelopment"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

recapDevelopmentN :: Genesis
recapDevelopmentN = recapDevelopment0
    & txChainIds .~ mkChainIdRange 1 9
    & coinbase ?~ devNGrants

recapDevelopmentKAD :: Genesis
recapDevelopmentKAD = recapDevelopment0
    & txChainIds .~ mkChainIdRange 10 19
    & coinbase ?~ devnetKadOps
    & keysets .~ Nothing
    & allocations .~ Nothing
    & namespaces ?~ devNs
    & coinContract .~ [fungibleAssetV1, fungibleAssetV2, coinContractV2Install, gasPayer]

-- ---------------------------------------------------------------------- --
-- Devnet - Development

fastDevelopment0 :: Genesis
fastDevelopment0 = Genesis
    { _version = Development
    , _tag = "Development"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

fastDevelopmentN :: Genesis
fastDevelopmentN = fastDevelopment0
    & txChainIds .~ mkChainIdRange 1 19
    & coinbase ?~ devNGrants

devNs2 :: FilePath
devNs2 = "pact/genesis/ns-v2.yaml"

devNs :: FilePath
devNs = "pact/genesis/ns-v1.yaml"

devKeysets :: FilePath
devKeysets = "pact/genesis/devnet/keysets.yaml"

dev0Grants :: FilePath
dev0Grants = "pact/genesis/devnet/grants0.yaml"

devNGrants :: FilePath
devNGrants = "pact/genesis/devnet/grantsN.yaml"

devAllocations :: FilePath
devAllocations = "pact/genesis/devnet/allocations.yaml"

devnetKadOps :: FilePath
devnetKadOps = "pact/genesis/devnet/kad-ops-grants.yaml"

-- ---------------------------------------------------------------------- --
-- CPM test versions

instantCPM0 :: Genesis
instantCPM0 = Genesis
    { _version = instantCpmTestVersion petersenChainGraph
    , _tag = "InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

instantCPMN :: Genesis
instantCPMN = instantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

pact5InstantCPM0 :: Genesis
pact5InstantCPM0 = Genesis
    { _version = pact5InstantCpmTestVersion petersenChainGraph
    , _tag = "Pact5InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

pact5InstantCPMN :: Genesis
pact5InstantCPMN = pact5InstantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

pact53TransitionCPM0 :: Genesis
pact53TransitionCPM0 = Genesis
    { _version = pact53TransitionCpmTestVersion petersenChainGraph
    , _tag = "Pact53TransitionTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

pact53TransitionCPMN :: Genesis
pact53TransitionCPMN = pact53TransitionCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

quirkedPact5InstantCPM0 :: Genesis
quirkedPact5InstantCPM0 = Genesis
    { _version = quirkedGasPact5InstantCpmTestVersion petersenChainGraph
    , _tag = "QuirkedGasPact5InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

quirkedPact5InstantCPMN :: Genesis
quirkedPact5InstantCPMN = quirkedPact5InstantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

fastTimedCPM0 :: Genesis
fastTimedCPM0 = Genesis
    { _version = fastForkingCpmTestVersion petersenChainGraph
    , _tag = "FastTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just fastNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

fastTimedCPMN :: Genesis
fastTimedCPMN = fastTimedCPM0
    & txChainIds .~ mkChainIdRange 1 9
    & coinbase ?~ fastNGrants

fastNs :: FilePath
fastNs = "pact/genesis/ns-v1.yaml"

fastKeysets :: FilePath
fastKeysets = "pact/genesis/devnet/keysets.yaml"

fast0Grants :: FilePath
fast0Grants = "pact/genesis/devnet/grants0.yaml"

fastNGrants :: FilePath
fastNGrants = "pact/genesis/devnet/grantsN.yaml"

fastAllocations :: FilePath
fastAllocations = "pact/genesis/devnet/allocations.yaml"

-- ---------------------------------------------------------------------- --
-- Vootaa networks

mono0 :: Genesis
mono0 = Genesis
  { _version = Mono
  , _tag = "Mono"
  , _txChainIds = onlyChainId 0
  , _coinbase = Nothing
  , _keysets = Nothing
  , _allocations = Nothing
  , _namespaces = Just vootaaMonoNs
  , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6Mono, gasPayer]
  }

triad0 :: Genesis
triad0 = Genesis
  { _version = Triad
  , _tag = "Triad"
  , _txChainIds = onlyChainId 0
  , _coinbase = Nothing
  , _keysets = Just vootaaTriadKeysets
  , _allocations = Just vootaaTriadAllocations
  , _namespaces = Just vootaaTriadNs
  , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
  }

triadN :: Genesis
triadN = triad0
  & txChainIds .~ mkChainIdRange 1 2
  & coinbase .~ Nothing
  & keysets .~ Nothing
  & allocations .~ Nothing

icosa0 :: Genesis
icosa0 = Genesis
  { _version = Icosa
  , _tag = "Icosa"
  , _txChainIds = onlyChainId 0
  , _coinbase = Nothing
  , _keysets = Just vootaaIcosaKeysets
  , _allocations = Just vootaaIcosaAllocations
  , _namespaces = Just vootaaIcosaNs
  , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
  }

icosaN :: Genesis
icosaN = icosa0
  & txChainIds .~ mkChainIdRange 1 19
  & coinbase .~ Nothing
  & keysets .~ Nothing
  & allocations .~ Nothing

vootaaMonoNs :: FilePath
vootaaMonoNs = "pact/genesis/vootaa/mono/ns.yaml"

vootaaTriadKeysets :: FilePath
vootaaTriadKeysets = "pact/genesis/vootaa/triad/keysets.yaml"

vootaaTriadAllocations :: FilePath
vootaaTriadAllocations = "pact/genesis/vootaa/triad/allocations.yaml"

vootaaTriadNs :: FilePath
vootaaTriadNs = "pact/genesis/vootaa/triad/ns.yaml"

vootaaIcosaKeysets :: FilePath
vootaaIcosaKeysets = "pact/genesis/vootaa/icosa/keysets.yaml"

vootaaIcosaAllocations :: FilePath
vootaaIcosaAllocations = "pact/genesis/vootaa/icosa/allocations.yaml"

vootaaIcosaNs :: FilePath
vootaaIcosaNs = "pact/genesis/vootaa/icosa/ns.yaml"

