module Chainweb.BlockHeader.Genesis.Icosa0Payload (payloadBlock) where

import Chainweb.Payload
import qualified Chainweb.BlockHeader.Genesis.Development0Payload as Development

payloadBlock :: PayloadWithOutputs
payloadBlock = Development.payloadBlock
