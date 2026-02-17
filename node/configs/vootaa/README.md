# Vootaa Network Profiles

This folder stores static network profile templates for the Vootaa rollout.

Profiles are grouped by network tier and topology:

- Devnet: `mono-devnet`, `triad-devnet`, `icosa-devnet`
- Mainnet: `mono-mainnet`, `triad-mainnet`, `icosa-mainnet`

Each template defines isolated ports, data directory suffixes, and PoW mode.

Scripts that resolve runtime targets:

- `Scripts/devnet_profiles.sh`
- `Scripts/devnet_acceptance_gate.sh`
- `Scripts/devnet_e2e_flow.sh`
