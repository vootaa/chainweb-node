{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Test.BlockHeader.Genesis
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Test.BlockHeader.Genesis ( tests ) where

import Control.Lens

import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties)

-- internal modules

import Chainweb.BlockHash (encodeBlockHash)
import Chainweb.BlockHeader
import Chainweb.Difficulty
import Chainweb.Test.Utils (golden)
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Icosa
import Chainweb.Version.IcosaDev
import Chainweb.Version.Mono
import Chainweb.Version.MonoDev
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Triad
import Chainweb.Version.TriadDev

---

-- FIXME This doesn't warn of incomplete pattern matches upon the addition of a
-- new `ChainwebVersion` value!
tests :: TestTree
tests = testGroup "Chainweb.Test.BlockHeader.Genesis"
    [ testGroup "genesis header golden tests" $ blockHashTest <$>
        [ RecapDevelopment
        , Mono
        , Triad
        , Icosa
        , MonoDev
        , TriadDev
        , IcosaDev
        ]
    , graphTransitionTargetTests
    ]

blockHashes :: HM.HashMap ChainId BlockHeader -> BL.ByteString
blockHashes =
    BB.toLazyByteString . foldMap (hash . snd) . sortBy (compare `on` fst) . HM.toList
  where
    hash :: BlockHeader -> BB.Builder
    hash = BB.byteString . B64U.encode . runPutS . encodeBlockHash . view blockHash

blockHashTest :: ChainwebVersion -> TestTree
blockHashTest v = golden (sshow v <> "-block-hashes") $
    pure $ blockHashes $ genesisBlockHeaders v

-- -------------------------------------------------------------------------- --
-- Graph Transition Targets

graphTransitionTargetTests :: TestTree
graphTransitionTargetTests = testGroup "graph transition genesis targets"
    [ testProperties "mono chains have max target" $
        forChain Mono maxTarget . unsafeChainId <$> [0]
    , testProperties "triad chains have max target" $
        forChain Triad maxTarget . unsafeChainId <$> [0..2]
    , testProperties "icosa chains have max target" $
        forChain Icosa maxTarget . unsafeChainId <$> [0..19]
    , testProperties "mono-dev chains have max target" $
        forChain MonoDev maxTarget . unsafeChainId <$> [0]
    , testProperties "triad-dev chains have max target" $
        forChain TriadDev maxTarget . unsafeChainId <$> [0..2]
    , testProperties "icosa-dev chains have max target" $
        forChain IcosaDev maxTarget . unsafeChainId <$> [0..19]
    ]

  where
    forChain v target cid = (show cid, v ^?! versionGenesis . genesisBlockTarget . atChain cid === target)
