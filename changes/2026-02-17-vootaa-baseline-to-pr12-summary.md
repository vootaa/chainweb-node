# Vootaa baseline-to-PR12 delivery summary

Date: 2026-02-17
Scope: Baseline snapshot (2026-02-14) to PR-12 completion

## Baseline reference

- Approved plan: `Docs/20260214-Vootaa-Minimal-Approved-for-Coding/Vootaa-Minimal-Coding-Implementation-Plan-Approved.md`
- PR checklist: `Docs/20260214-Vootaa-Minimal-Approved-for-Coding/Vootaa-PR-Level-Execution-Checklist.md`

## Delivered milestones

1. PR-01 / PR-02
   - Added and registered `mono`, `triad`, `icosa` versions.
   - Version codes and graph orders aligned with frozen requirements.

2. PR-03
   - Node PoW path switched to `Blake2b_256`.
   - Domain prefix isolation added: `Vootaa-POW-PS1|<versionName>`.

3. PR-05
   - Genesis payload wiring integrated into `cwtools/ea` for three versions.
   - Generated payload modules connected to version modules.

4. PR-08
   - Frozen preallocation policy implemented:
     - mono: no preallocation
     - triad/icosa: 50% preallocation on chain 0 only
   - Mono-specific coin install path added with cross-chain transfer disabled.

5. PR-09 / PR-10 (cross-repo compatibility)
   - `chainweb-data` and `kda-tool` updated for version recognition and defaults.

6. PR-11 / PR-12
   - Scripted development flow expanded with gate + e2e chain.
   - Legacy targets blocked in new flow.
   - 6-target matrix (mono/triad/icosa + mono-dev/triad-dev/icosa-dev) and profile templates added.
   - Isolation checks enforced for ports, data directories, and config files.

## Artifacts and traceability

- Node profile templates: `node/configs/vootaa/`
- Script entry points:
  - `Scripts/devnet_profiles.sh`
  - `Scripts/devnet_acceptance_gate.sh`
  - `Scripts/devnet_e2e_flow.sh`
  - `Scripts/one_click_dev_flow.sh`
- Audit records: `Docs/20260214-Vootaa-Minimal-Approved-for-Coding/records/`

## Notes

This file is a user-visible change summary for release/audit traceability.
It does not redefine consensus behavior; it references already delivered PR scopes.
