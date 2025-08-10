# UI Components Migration Guide

This guide codifies the approach we used to fix the UIWrapped Button and its tests, and provides a repeatable playbook to upgrade the rest of the UI components to the canonical patterns.

## Goals
- Provide a stable, app-level API for UI components (wrapper layer) while using vendor primitives (SaladUI) internally
- Ensure components compile and test cleanly with property-based tests
- Keep vendor coupling minimal and isolated

## Terminology
- Canonical UI: `RivaAshWeb.Components.UI.*` components (in `lib/riva_ash_web/components/ui/*`)
- App-level wrappers: `RivaAshWeb.Components.UIWrapped.*` components (in `lib/riva_ash_web/components/ui_wrapped/*`) that expose a stable API and may map to vendor components internally
- Vendor primitives: `SaladUI.*` components (from the salad_ui dep)

## High-level patterns
1) Do not import all UI components globally in `web.ex` to avoid naming conflicts (e.g., `card/1`). Prefer explicit imports in each module or alias the wrapper module used in HEEx.
2) Wrapper components may map app-level props (e.g., variants, sizes) to vendor props using small pure functions.
3) Loading indicators: prefer an app-owned spinner (`UI.Spinner`), or inline minimal markup as a fallback, rather than calling vendor spinner directly from inside another wrapper.
4) Property tests must render via `~H` so required slots (e.g., `inner_block`) are provided correctly.

## Example: UIWrapped Button (final pattern)
- File: `lib/riva_ash_web/components/ui_wrapped/button.ex`
- Behavior: Stable props, maps to `SaladUI.Button.button/1`, renders a small inline spinner when `loading=true`.

### Minimal render excerpt
```
<SaladUI.Button.button ... disabled={@disabled or @loading} ...>
  <%= if @loading do %>
    <span class="mr-2 w-4 h-4 inline-block align-middle border-2 border-current border-t-transparent rounded-full animate-spin" aria-hidden="true"></span>
  <% end %>
  <%= render_slot(@inner_block) %>
</SaladUI.Button.button>
```

### Property test pattern
- Render via `~H` to provide the required slot:
```
assigns = %{props: props}
html = rendered_to_string(~H"""
  <Button.button variant={@props.variant} size={@props.size}
                 loading={@props.loading} disabled={@props.disabled}>
    <%= @props.label %>
  </Button.button>
""")
```
- Assertions: label present; `animate-spin` when loading; `disabled` attribute when disabled or loading; contains `<button`.

## Migration steps per component
1) Create wrapper module (if applicable)
- Path: `lib/riva_ash_web/components/ui_wrapped/<component>.ex`
- `use Phoenix.Component`
- Define app-level attrs and acceptable values
- Provide mapping functions from app API to vendor API (variants, sizes, etc.)
- Render vendor component inside (~H) with mapped props
- Handle `disabled` and `loading` consistently

2) Tests
- Unit tests: `test/riva_ash_web/components/ui_wrapped/<component>_test.exs`
  - Render with `~H` and assert basic output (label/content, tag presence, class hooks)
- Property tests: `test/riva_ash_web/components/ui_wrapped/<component>_property_test.exs`
  - Use StreamData to generate valid prop combinations
  - Always render via `~H` so slots are provided
  - Assert invariants: no crash; expected attributes/classes from states

3) Preferred imports/usage in LiveViews and templates
- To avoid name clashes, explicitly import only what you need:
  - `import RivaAshWeb.Components.UI.Button` for canonical Button
  - OR alias the wrapper module and call it by module-qualified name:
    - `<Button.button ...>...</Button.button>` with `alias RivaAshWeb.Components.UIWrapped.Button`
- Avoid globally importing `RivaAshWeb.Components.UI` in `web.ex` (keeps ambiguous names out).

4) Loading spinners
- Preferred: use `RivaAshWeb.Components.UI.Spinner` where you need a standalone spinner.
- Inside another wrapper (e.g., Button), either:
  - Render our `UI.Spinner` with a very small footprint, or
  - Use inline minimal spinner markup (as shown above) to avoid adding another dependency boundary inside the wrapper.

5) Mapping guidelines
- Variants: expose a stable app-level set; map to vendor variants via small functions. Default unknown values to a safe default.
- Sizes: same pattern as variants. Provide a backwards-compatible alias if needed (`"md" -> "default"`).
- Disabled: If `loading`, force `disabled` attribute for predictable UX.
- Class merging: Always include the wrapper’s base classes, then append `@class` from assigns.

## Component checklist
For each component (Button, Input, Select, Checkbox, Textarea, Badge, Card*, Alert, Spinner):
- [ ] Wrapper exists under `ui_wrapped/` (if we want a wrapper for it)
- [ ] Canonical UI component exists under `ui/` and matches the design tokens
- [ ] Mapping functions for variants/sizes implemented (if applicable)
- [ ] Inline/default spinner or UI.Spinner usage decided (if applicable)
- [ ] Unit tests added in `test/.../<component>_test.exs`
- [ ] Property tests added in `test/.../<component>_property_test.exs`
- [ ] All tests pass: `mix test test/riva_ash_web/components/ui_wrapped/<component>* --seed 0`
- [ ] Credo passes: `mix credo --strict`

Note: Card is a family (card, card_header, card_title, card_description, card_content, card_footer). Ensure consistent naming and import ergonomics; avoid alias conflicts by importing the exact functions used, or using module-qualified tags.

## Commands
- Compile: `cd packages/riva_ash && mix compile`
- Run just the component tests: `mix test test/riva_ash_web/components/ui_wrapped/<component>* --seed 0`
- Credo: `mix credo --strict`

## Guardrails
- Keep wrapper APIs minimal and stable; do not leak vendor prop names upstream.
- Prefer small pure mapping functions and a single render function per component.
- Avoid global imports that create ambiguous component names.
- Ensure property tests cover edge values (unknown variant/size revert to defaults).

## Where to look
- Canonical UI components: `lib/riva_ash_web/components/ui/`
- Wrapper components: `lib/riva_ash_web/components/ui_wrapped/`
- Tests: `test/riva_ash_web/components/ui_wrapped/`
- Vendor dep: `{:salad_ui, "~> 1.0.0-beta"}` in `packages/riva_ash/mix.exs`
- Tailwind content includes vendor: `packages/riva_ash/assets/tailwind.config.js`
- CSS imports include `salad_ui.css`: `packages/riva_ash/assets/css/{app,storybook}.css`

## AI agent playbook (per component)
1) Create/verify wrapper module in `ui_wrapped/` with stable attrs and mapping functions
2) Render vendor component via ~H; handle `disabled` and `loading`
3) Write unit test and property test following the Button patterns
4) Run: `mix compile && mix test test/.../<component>* --seed 0 && mix credo --strict`
5) If there are naming collisions, remove broad imports and import only needed modules/functions
6) Commit with message: "feat(ui): add UIWrapped.<Component> with tests and mappings to SaladUI"

