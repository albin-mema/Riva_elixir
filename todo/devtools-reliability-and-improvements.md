# DevTools reliability and usefulness

Goal: Make DevTools pages render reliably and provide minimally useful data hooked to real telemetry/services (no dead stubs or 404 links). Only essentials, no feature bloat.

- [ ] Router and access
  - [ ] Verify /dev scope is enabled only in :dev and links render without 404s
  - [ ] Fix DevTools home links that point to disabled routes (GraphQL, etc.)
    - [ ] Hide /graphql button unless enabled
    - [ ] Keep /docs link

- [ ] Ash Inspector (ash_inspector_live.ex)
  - Issues: duplicate alias, telemetry handlers are stubs, unmatched clauses
  - [ ] Remove duplicate alias lines and *_unmatched* placeholders
  - [ ] Implement minimal telemetry accumulation for queries/actions/policies
  - [ ] Add filter functions that actually filter by resource/action
  - [ ] Add unit tests to assert event handling updates assigns

- [ ] Business Context Inspector (business_context_live.ex)
  - Issues: depends on DevTools.BusinessContextService (not found), unmatched clauses
  - [ ] Implement a minimal BusinessContextService or inline minimal logic
    - [ ] Load current user, businesses they own/belong to, employees list via Ash DB filters
  - [ ] Replace _unmatched placeholders; harden role badges
  - [ ] Tests: mount with and without user session, verifies assigns and renders

- [ ] Reactor Visualizer (reactor_visualizer_live.ex)
  - Issues: depends on DevTools.ReactorService; execution is mock only
  - [ ] Implement minimal ReactorService listing real reactors (lib/riva_ash/reactors/*)
  - [ ] Keep execution mock but wire step events to UI (good enough for dev)
  - [ ] Tests: rendering with available_reactors, toggles code view, step messages

- [ ] Performance Dashboard (performance_dashboard_live.ex)
  - Issues: telemetry handlers and update_* functions are stubs; unmatched
  - [ ] Implement basic handlers to track recent response times and slow Ash queries
  - [ ] Implement update_memory_usage/connection_count using :erlang.memory/1 and Phoenix.PubSub presence or Endpoint metrics
  - [ ] Tests: simulate telemetry messages and assert assigns update

- [ ] Test Data Generator (test_data_generator_live.ex)
  - Issues: several *_unmatched* placeholders, quick reservation unimplemented
  - [ ] Implement quick reservation creation using existing resources (Item, Client)
  - [ ] Replace String.to_existing_atom usage on user-provided values to safe parsing
  - [ ] Harden count_records (avoid rescue on broad; use Ash count if available)
  - [ ] Tests: quick generate flows produce success logs; options parsing works

- [ ] Policy Visualizer (policy_visualizer_live.ex)
  - Issues: depends on DevTools.PolicyService; trace result returns mock map; unmatched placeholders
  - [ ] Implement minimal PolicyService that builds a tree from Ash policies metadata (basic)
  - [ ] Replace get_trace_result with lookup into @policy_trace; default nil
  - [ ] Tests: simulate a minimal tree and render badges/colors correctly

- [ ] Docs
  - [ ] Update docs/devtools.md to reflect working tools and how to use them

Notes:
- Keep everything behind Mix.env() == :dev as already done
- Favor correctness and minimal utility over ambitious UIs

