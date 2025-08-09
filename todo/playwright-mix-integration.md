# Playwright + Mix integration (developer UX)

Goal: Ensure browser tests (headed and headless) run directly via `mix test` without ad-hoc shell scripts, matching project preference.

- [ ] Verify current behavior
  - [ ] `packages/riva_ash/test/*_browser_*_test.exs` exist and run
  - [ ] Ensure no reliance on `scripts/test-browser-demo.sh` for real tests

- [ ] Headed vs headless toggle via env
  - [ ] Respect PLAYWRIGHT_HEADLESS env var in tests

- [ ] Update docs and scripts
  - [ ] Root README and docs/testing-guidelines.md reflect `mix test` usage
  - [ ] Remove demo-only scripts from recommended paths (keep for examples only)

- [ ] CI-ready commands
  - [ ] Document `pnpm test:browser:headless` maps to `MIX_ENV=test mix test --include browser`
  - [ ] Ensure `mix compile` and `mix credo` are part of the flow

