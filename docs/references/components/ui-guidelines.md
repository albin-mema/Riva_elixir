# UI Guidelines (Condensed)

## Canonical components
- Use `RivaAshWeb.Components.UI.*` for all new UI. `Atoms.*` are deprecated (wrappers exist; do not use for new code).
- Molecules/Organisms compose UI primitives internally. Storybook shows components (not pages).

## Migration (Atoms → UI)
- Phase 1: New code uses UI.* only (Credo flags Atoms.*)
- Phase 2: Atoms.* delegate to UI.*, deprecation warnings
- Phase 3: Remove Atoms.*

## Usage patterns
- Prefer explicit aliases to avoid name clashes:
```elixir
alias RivaAshWeb.Components.UI.Button, as: UIButton
alias RivaAshWeb.Components.UI.Input, as: UIInput
```
- Migrate old code by replacing Atoms.* with UI.* equivalents.

## Tables, forms, styling
- Tables: Flop (+ flop_phoenix) for sort/filter/pagination
- Forms: AshPhoenix.Form for validation and changes
- Styling: Tailwind + design tokens; avoid custom CSS

## Accessibility & testing
- ARIA labels/roles, focus-visible, keyboard nav
- Tests per component: unit + property (variants, sizes, disabled/loading)
- LiveView interactions: Phoenix testing; optional small Playwright smoke

## Wrapper strategy
- Thin app-level API over vendor (SaladUI); map variants/sizes via small pure fns
- If loading, force disabled; provide light inline spinner or UI.Spinner

## Common snippets
```elixir
# Use UI components directly
alias RivaAshWeb.Components.UI.Button, as: UIButton
~H"""
<UIButton.button variant="primary" size="lg">Save</UIButton.button>
"""
```

```elixir
# Molecule using UI primitives internally
alias RivaAshWeb.Components.UI.{Input, Text}
~H"""
<div class="space-y-2">
  <Text.text variant="label"><%= @label %></Text.text>
  <Input.input field={@field} placeholder={@placeholder} />
</div>
"""
```
