module Chainweb.BlockHeader.Genesis.Triad1to2Payload (payloadBlock) where

import Chainweb.Payload
import qualified Chainweb.BlockHeader.Genesis.Development1to19Payload as Development

payloadBlock :: PayloadWithOutputs
payloadBlock = Development.payloadBlock
