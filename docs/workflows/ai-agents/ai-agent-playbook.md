# AI Agent Playbook

This playbook defines how automated agents should propose and apply improvements safely across the Riva Ash codebase.

## Goals
- Improve code quality and consistency without changing behavior
- Enforce project rules from .airules and styleguide.md
- Keep changes small, reviewable, and verifiable

## Scope
- Applies to Elixir code in packages/riva_ash and related test/support files
- Non-Elixir assets (JS/CSS) are out of scope unless a task explicitly includes them

## Allowed changes (safe by default)
- Formatting and style (no semantic changes)
- Remove unused aliases/imports/variables; organize aliases
- Convert case statements to pattern-matching function heads where behavior is preserved
- Pipeline cleanups and small refactors to achieve single-level of abstraction
- Add/clarify @moduledoc, @doc, @spec, types, and comments
- Replace in-memory filtering with correct Ash DB-level filtering when tests exist or are added
- Credo autofixes that are semantics-preserving

## Prohibited changes (require explicit task + tests + review)
- Public API changes, module renames/moves, signature changes
- Ash resource actions, policies, authentication/authorization logic
- Database schema/migrations; data migrations; seed changes
- LiveView behavioral changes (assigns, events, navigation) or UI-breaking changes
- Performance-affecting changes (caching, concurrency, ETS, processes) without benchmarks and tests

## Verification steps for every PR
From packages/riva_ash directory:

1. mix compile (no new warnings)
2. mix credo --strict (no new issues introduced)
3. mix test — preferably targeted to affected areas; add property-based tests when suitable
4. If Ash queries changed: add tests that assert DB-level filtering (avoid in-memory filtering)

## Batching and review
- Keep PRs small (≤20 files, one domain area)
- Include a rationale referencing relevant sections of .airules and styleguide.md
- Provide a summary of changes and any risk assessment
- Prefer a dry-run (diff-only) preview for large-scale edits for human approval before applying

## Workflow for agents
1. Read .airules and styleguide.md, and identify relevant sections
2. Propose small, semantics-preserving changes with a clear diff
3. Run compile/credo/tests locally; include results in the PR description
4. If any check fails, reduce scope or revert changes

## Examples
- Convert in-memory filter to Ash query filter (include test)
- Replace a case with function head pattern-matching (no behavior change)
- Extract a 10-line inline block into a named private function to preserve single-level abstraction
- Remove unused aliases and fix alias grouping according to styleguide

## Machine-readable rules
See .ai-agent.yml for programmatic enforcement that tools can read.

