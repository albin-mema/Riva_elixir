# Documentation Alignment Checklist (Reservo)

Use this checklist to audit and improve the docs so they align with Reservo’s mission: a universal, domain-agnostic, full-day reservation platform with constant pricing, cash-only payment tracking, LiveView UI, and Ash policies.

## A. Naming and Mission
- [ ] Project name uses "Reservo" everywhere (no "Riva Ash" / "Reservation System" leftovers)
- [ ] Root README clearly states mission and core principles (full-day only, cash tracking, equal days, grid layout)
- [ ] Docs/README intro matches the mission

## B. Domain Alignment
- [ ] Remove/avoid "document management" or archival-first product language in root/docs
- [ ] Pricing docs use constant base price + business exceptions (no weekday/weekend differentials)
- [ ] Reservation docs emphasize full-day and consecutive multi-day only; no time-slot scheduling
- [ ] Layout docs use row/column grid language, not x,y free-form coordinates
- [ ] Payments described as cash-only tracking (no card gateways)

## C. Tech/Stack Consistency
- [x] Ash policies and SimpleSat mentioned where permissions are discussed
- [x] LiveView + Tailwind as UI default; tables use Flop (not React-first)
- [x] JSON:API and GraphQL are both referenced and wired in router/domain docs
- [x] PaperTrail and soft delete (archival) described as audit/retention features, not a document system

## D. Setup/Paths/Env
- [ ] Paths reflect repo layout: packages/riva_ash is the Phoenix app
- [ ] Commands use the correct directories (cd Riva_Ash, cd packages/riva_ash)
- [ ] Env var names consistent across README, docs, and example configs
- [ ] Docker compose service names/ports match docs

## E. UI/UX Guidelines
- [ ] LiveView component guidelines present; Tailwind usage consistent
- [ ] Foreign keys in admin forms described as selects
- [ ] Live debugger tool and storybook references present
- [ ] Flop-based tables and pagination notes included

## F. Testing Guidance
- [ ] Property-based testing emphasized (StreamData)
- [ ] Phoenix testing for LiveView/UI included
- [ ] Authentication enabled in tests rather than disabled
- [ ] How to run unit/integration/property tests documented

## G. Permissions and Onboarding
- [x] Centralized permission constants documented for Ash policies
- [x] Auto-admin on first business creation documented
- [x] Example policy matrices align with domain roles

## H. Files to Update/Add
- [ ] Root README (mission, principles, quick start) — Updated
- [ ] docs/README (index + mission alignment) — Updated
- [ ] docs/RESERVATION_SYSTEM_ASSESSMENT (remove weekday/weekend, time-slots, coordinates) — Updated
- [ ] docs/USER_ACTIONS (rename to Reservo and align pricing rules) — Updated
- [ ] docs/DEVELOPMENT_SETUP (paths, envs) — Updated
- [ ] packages/riva_ash/README (name, env-based DB config, paths) — Updated
- [ ] docs/CONTRIBUTING (rename and generalize) — Updated
- [ ] Create a "Permissions Matrix" doc if missing
- [ ] Create a "Reservation Center" design doc (or link) — Present as docs/reservation-center-design.md

## I. Open Issues Found During Review
- [ ] Remove weekday/weekend pricing fields from any ERD diagrams and examples
- [ ] Replace references to time-slot validation with full-day validation
- [ ] Replace coordinates with grid row/column language
- [ ] Ensure GraphQL exposure is documented and consistent with code
- [ ] Replace any React-first table guidance with LiveView+Flop
- [ ] Standardize DB_NAME env to "reserv0_dev" or preferred value across docs

## J. Proposed Next Steps (from latest review)

1) Standardize naming and paths
- [ ] Confirm preferred dev DB name (e.g., reserv0_dev) and standardize across docs and examples
- [ ] Normalize repository paths/commands (Riva_Ash root, packages/riva_ash)
- [ ] Sweep and replace remaining “Riva Ash”/“Reservation System” strings with “Reservo” where appropriate

2) Strengthen mission alignment in docs
- [ ] Add MISSION_AND_SCOPE.md link to root README and docs/README
- [ ] Add a short mission banner to docs index pages
- [ ] Update any remaining pricing/scheduling/layout examples to reflect: full-day, equal days, grid

3) Permissions & policies
- [ ] Add a concise Permissions Matrix narrative doc aligned with Ash policies + SimpleSat
- [ ] Cross-link from CONTRIBUTING.md and USER_ACTIONS.md

4) API surfaces
- [ ] Add a GraphQL Quick Start snippet (how to query/mutate) to README and docs/README
- [ ] Verify JSON:API routes and GraphQL blocks match docs references

5) UI guidance
- [ ] Verify all table references recommend LiveView + Flop
- [ ] Keep React mentions limited to live_react integration where explicitly desired

6) Verification passes
- [ ] Run mix docs/mix credo and fix any doc-related warnings
- [ ] Link-check docs (local) to prevent broken anchors

Owner: Documentation Maintainers
Last updated: 2025-08-11

