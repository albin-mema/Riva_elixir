# UI Components Migration (Condensed)

Goals: stable app-level API via wrappers, clean tests, minimal vendor coupling.

## Patterns
- Avoid global imports in `web.ex`; prefer explicit imports/aliases
- Map app props (variants/sizes) to vendor via small pure fns
- Loading: prefer `UI.Spinner` or minimal inline markup
- Property tests: always render via `~H` to supply slots

## Example: Button wrapper
- `lib/riva_ash_web/components/ui_wrapped/button.ex`
- Maps to `SaladUI.Button.button/1`, inline spinner when `loading`

Render excerpt:
```
<SaladUI.Button.button disabled={@disabled or @loading}>
  <%= if @loading, do: ~s(<span class="... animate-spin" aria-hidden="true"></span>) %>
  <%= render_slot(@inner_block) %>
</SaladUI.Button.button>
```

Property test sketch:
```
html = rendered_to_string(~H"""
  <Button.button variant={@props.variant} size={@props.size}
                 loading={@props.loading} disabled={@props.disabled}>
    <%= @props.label %>
  </Button.button>
""")
```

## Migration steps
1) Create wrapper under `ui_wrapped/` with stable attrs and mapping fns
2) Tests: unit + property (use StreamData); render via `~H`
3) Usage: import specific UI modules or alias wrappers; avoid broad imports
4) Spinners: `UI.Spinner` or inline; loading forces `disabled`
5) Mapping: variants/sizes map with safe defaults; always merge base classes + `@class`

## Checklist (per component)
- Wrapper exists (if needed) and canonical UI component exists
- Mapping functions in place; spinner policy decided
- Tests present (unit + property); `mix test` green
- `mix credo --strict` passes

## Commands
- Compile: `cd packages/riva_ash && mix compile`
- Test (component): `mix test test/riva_ash_web/components/ui_wrapped/<component>* --seed 0`
- Credo: `mix credo --strict`

## Locations
- UI: `lib/riva_ash_web/components/ui/`
- Wrappers: `lib/riva_ash_web/components/ui_wrapped/`
- Tests: `test/riva_ash_web/components/ui_wrapped/`
- Vendor: salad_ui dep; Tailwind content includes vendor; CSS imports salad_ui.css

