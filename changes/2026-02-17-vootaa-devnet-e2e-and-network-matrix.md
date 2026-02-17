# Vootaa network matrix, isolation, and end-to-end flow

Date: 2026-02-17

## Summary

This change set finalizes operational scripts and templates for Vootaa network rollout with a complete end-to-end execution path.

## What changed

- Added network profile matrix support in `Scripts/devnet_profiles.sh`:
  - devnet: `mono-devnet`, `triad-devnet`, `icosa-devnet`
  - mainnet: `mono-mainnet`, `triad-mainnet`, `icosa-mainnet`
- Added tier-aware resolution for shorthand targets (`mono|triad|icosa` + `VOOTAA_NETWORK_TIER`).
- Hardened script-level isolation checks:
  - distinct service/p2p/miner ports
  - distinct data directories
  - distinct runtime config files
- Added full end-to-end script chain: `Scripts/devnet_e2e_flow.sh`.
- Updated one-click flow to include `e2e` stage.
- Added chainweb-node profile templates under `node/configs/vootaa/`.

## Behavior and compatibility

- Legacy targets (`mainnet01`, `testnet04`, `mainnet`, `testnet`) remain blocked in new flow.
- Existing `mono-devnet|triad-devnet|icosa-devnet` behavior remains supported.

## Verification

- `bash Scripts/devnet_acceptance_gate.sh`
- `VOOTAA_DEVNET_TARGET=triad-mainnet bash Scripts/devnet_profiles.sh`
- `VOOTAA_DEVNET_TARGET=icosa-mainnet bash Scripts/devnet_profiles.sh`
- `bash Scripts/devnet_e2e_flow.sh` (configurable with env flags)
