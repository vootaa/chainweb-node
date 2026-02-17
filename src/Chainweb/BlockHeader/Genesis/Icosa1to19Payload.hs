module Chainweb.BlockHeader.Genesis.Icosa1to19Payload (payloadBlock) where

import Chainweb.Payload
import qualified Chainweb.BlockHeader.Genesis.Development1to19Payload as Development

payloadBlock :: PayloadWithOutputs
payloadBlock = Development.payloadBlock
