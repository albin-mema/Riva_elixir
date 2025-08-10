## Atomic Design TODO: Components, Combinations, Templates, and Pages

Goal: Ensure every UI element adheres to Atomic Design (Tokens → Atoms → Molecules → Organisms → Templates → Pages). Pages must be composed of templates and lower-level components; do not introduce page-unique components. Test smaller components themselves and the combinations thereof. Improve existing components where noted.

References
- docs/styleguide.md, docs/architecture-guidelines.md, docs/component_library_specifications.md, docs/atomic_components_migration_plan.md
- Storybook directories: packages/riva_ash/storybook/{atoms,molecules,organisms,templates,combinations}
- Navigation map: docs/ui_routes_and_navigation.md

Conventions
- Naming: snake_case for component modules; consistent prop naming across layers
- Theming: only use design tokens; no hard-coded spacing/colors
- Accessibility first: keyboard navigation, focus management, ARIA roles/labels, color contrast
- Testing: unit tests for atoms/molecules; combinations property-based tests; visual regression in Storybook; Playwright navigation flows for pages

---

### Design Tokens and Foundations
- [ ] Audit and consolidate design tokens (colors, spacing, typography, radii, shadows)
- [ ] Ensure Storybook "Design Tokens" page is the single source of truth
- [ ] Dark mode tokens and surface layering (background/foreground/overlay)
- [ ] Motion tokens: standard durations/easings for hover/focus/enter/exit
- [ ] Global accessibility audit of tokens (contrast, focus rings)

---

### Atoms (review existing and fill gaps)
Existing stories: avatar, badge, button, checkbox, date_picker, icon, input, radio, select, spinner, text, text_input, time_picker, toggle, tooltip, chat_input, chat_message, room_list_item

Refactor/Improve
- [ ] Standardize props and variants across atoms (size, variant, disabled)
- [ ] Ensure all atoms consume tokens (no raw hex/px)
- [ ] Add focus-visible states; remove outline: none hacks
- [ ] Keyboard and screen-reader support (labels, aria-*)
- [ ] Snapshot/visual tests in Storybook for key states

Additions (new atoms)
- [ ] Link (inline, external, muted)
- [ ] IconButton (accessible icon-only button)
- [ ] BadgeCounter (with max+)
- [ ] Divider/Separator (horizontal/vertical)
- [ ] Kbd (for shortcuts)
- [ ] Tag/Chip (removable)
- [ ] Skeleton (lines, blocks, avatar)
- [ ] Tooltip improvements (direction, collision, delay)

---

### Molecules (review existing and fill gaps)
Existing stories: action_menu, breadcrumb_nav, card, chat_messages_list, confirm_dialog, empty_state, filter_panel, form_field, notification_toast, pagination, progress_bar, search_bar, status_indicator, tab_navigation

Refactor/Improve
- [ ] Unify card subcomponents (header/content/footer/title/description) usage
- [ ] Confirm dialog focus trap; escape/overlay click behavior
- [ ] Pagination keyboard support and aria-current
- [ ] Search bar with async loading and suggestions
- [ ] Breadcrumb responsive collapse and overflow

Additions
- [ ] CommandPalette (Cmd/Ctrl+K)
- [ ] DropdownMenu (menu + submenus, roving focus)
- [ ] Accordion/Disclosure
- [ ] Stepper (wizard steps)
- [ ] ToastProvider (queue, screen-reader polite live region)
- [ ] FileUpload (basic + drag/drop)
- [ ] DateRangePicker (composed from date_picker)
- [ ] Combobox (typeahead select)

---

### Organisms (review existing and fill gaps)
Existing stories: business_card, business_form, calendar_view, client_form, dashboard_stats, data_table, employee_form, header, item_form, layout_designer, page_header, permission_matrix, pricing_form, reservation_form, timeline_view

Refactor/Improve
- [ ] Header: slots for left/center/right; keyboard shortcuts; notification bell menu
- [ ] DataTable: column resizing, virtualization (optional), sticky header
- [ ] Forms: unify validation display; error summary pattern
- [ ] Timeline/Calendar: time scale tokens; selection accessibility
- [ ] PermissionMatrix: keyboard nav; bulk actions

Additions
- [ ] Sidebar (collapsible, nested groups, active trail)
- [ ] AppShell (Header + Sidebar + Content + Right rail slots)
- [ ] RightRail (context panel; slide-in)
- [ ] GlobalSearchResults (list + keyboard nav)
- [ ] InboxList (notifications/tasks)
- [ ] ActivityStream (filterable audit log)

---

### Templates
Existing stories: calendar_template, dashboard_template, detail_view_template, form_view_template, list_view_template

Refactor/Improve
- [ ] Standardize template props: title, actions, toolbar, tabs, breadcrumbs, sidebar content
- [ ] Ensure templates are page-agnostic; only accept data via slots/props

Additions
- [ ] WizardTemplate (multi-step with Stepper)
- [ ] SplitViewTemplate (list + detail, with RightRail)
- [ ] TwoPaneTemplate (sidebar navigation + content)
- [ ] FocusTemplate (auth/booking flow; no sidebar)

---

### Combinations (structured testing scenarios)
Existing stories: dashboard_header, data_table, form_with_validation, reservation_card, ui_component_showcase

Improvements
- [ ] Systematically generate combinations via property-based testing (sizes, variants, density)
- [ ] Add Axe accessibility checks to each combination
- [ ] Visual regression baselines for critical combinations

New combinations
- [ ] AppShell + Sidebar + Header with different nav configurations
- [ ] DataTable with filter_panel + pagination + empty_state
- [ ] Forms using form_field + validation + confirm_dialog
- [ ] CommandPalette + search_bar integration

---

### Pages (composition only; no page-unique components)
Rule: do not add “page” stories unless they are wired through templates and organisms; pages are validated via Playwright flows. See docs/ui_routes_and_navigation.md for the list.

Internal (existing)
- [ ] Dashboard (/dashboard) via DashboardTemplate + dashboard_stats
- [ ] Reservations (/reservations) via SplitViewTemplate + data_table + right_rail
- [ ] Inventory (/inventory) via TwoPaneTemplate + nested routes
- [ ] People (/people) via TwoPaneTemplate (tabs: Clients/Employees/Users)
- [ ] Finance (/finance) via List/Detail templates linking to payments/pricing
- [ ] Chat (/chat) via layout using chat_* atoms/molecules
- [ ] Settings (/settings) via TwoPaneTemplate + forms

Public/Client (proposed)
- [ ] Portal Home (/portal) via DashboardTemplate (client flavor)
- [ ] Browse (/portal/browse) via ListViewTemplate + filters
- [ ] Book (/portal/book/:id) via WizardTemplate
- [ ] My bookings (/portal/bookings) via List/Detail templates
- [ ] Account (/portal/account) via FormViewTemplate

Superadmin/Dev
- [ ] Admin (/admin) framed within AppShell
- [ ] ERD (/erd) framed within AppShell
- [ ] Dev tools (/dev/*) framed within AppShell

---

### Testing and Quality Tasks
- [ ] Storybook coverage: every atom/molecule/organism/template has stories for states (default, hover, focus, disabled, error)
- [ ] Property-based test generators for combinations (sizes, variants, density)
- [ ] Playwright: navigation flows for all pages; ensure rendering and no server errors

---



### Suggestions and Improvements
- [ ] Introduce a shared Overlay/Portal system (modal, drawer, tooltip anchor)
- [ ] Add RTL support and verify mirroring in sidebar and breadcrumb
- [ ] Keyboard shortcuts map: expose via Kbd and Help dialog
- [ ] Density control (comfortable/compact) via tokens
- [ ] Theming modes: light/dark/system; ensure tokens support both
- [ ] Adopt breadcrumb + page header pattern across all pages
- [ ] Introduce a Design Review checklist integrated with mix credo custom checks

---

### Tracking and Workflow
- [ ] Prioritize Sidebar, AppShell, and CommandPalette to unlock navigation redesign
- [ ] For each component: implement → storybook states → unit tests → a11y checks → add to combinations
- [ ] Update docs/.airules and styleguide.md with atomic rules and review checklist
- [ ] Run mix compile and mix credo on every change (enforced in CI)



---

### Composition Guide and Mobile Adaptation

Notation: A = Atom, M = Molecule, O = Organism, T = Template. Use tokens for spacing/color/typography/motion; ensure a11y states.

#### Atoms (intended composition)
- Link — A: text + icon (optional), focus-ring; tokens: color.semantic.link, underline style
- IconButton — A: button variant with icon-only, tooltip hook; tokens: size.xs/s/m, color.surface
- BadgeCounter — A: badge + text; tokens: color.semantic.info, radius.s
- Divider/Separator — A: rule line; tokens: color.border.muted, spacing.y
- Kbd — A: inline box around text; tokens: font.mono, radius.xs, color.surface/fg.muted
- Tag/Chip — A: container + text + close IconButton (optional); tokens: color.semantic.tag, radius.pill
- Skeleton — A: block/line/avatar placeholders; tokens: color.surface.skeleton, motion.pulse
- Tooltip (improved) — A: trigger wrapper + content; tokens: elevation.overlay, motion.fade/scale

#### Molecules (what they’re made of)
- action_menu — M: button/IconButton (trigger) + DropdownMenu list; a11y: roving focus
- breadcrumb_nav — M: Link atoms + separators; responsive collapse via overflow menu
- card — M: container + header (A: text, IconButton) + content + footer (Divider)
- chat_messages_list — M: chat_message atoms list + virtualizer (optional)
- confirm_dialog — M: overlay + text + button group; focus trap and escape handling
- empty_state — M: icon + title + description + primary/secondary Button
- filter_panel — M: form_field molecules + Tag/Chip summary + clear action
- form_field — M: label + input atom + help/error text; aria-describedby wiring
- notification_toast — M: Toast item (icon + text + dismiss IconButton)
- pagination — M: IconButton group + text; aria-current and keyboard support
- progress_bar — M: determinate/indeterminate bar; tokens: motion.smooth
- search_bar — M: input + IconButton + suggestions popover; async loading state
- status_indicator — M: dot/badge + text; tokens: color.semantic.status*
- tab_navigation — M: tab list + tab panels; roving focus + aria-controls
- CommandPalette — M: input + list + keyboard handler; global shortcut
- DropdownMenu — M: trigger + menu + submenu; typeahead + roving focus
- Accordion/Disclosure — M: button header + content region; animated height
- Stepper — M: steps list + progress + next/back buttons
- ToastProvider — M: Overlay/Portal + queue management + live region
- FileUpload — M: button/dropzone + file list + progress + remove actions
- DateRangePicker — M: two date_picker atoms + presets + apply button
- Combobox — M: input + listbox + option; ARIA combobox pattern

#### Organisms (composition from atoms/molecules)
- business_card — O: card + avatar + text + action_menu
- business_form — O: page_header + form_field groups + confirm_dialog
- calendar_view — O: timeline_view + date_picker + Toolbar (search_bar, filter_panel)
- client_form — O: page_header + form_field groups + validation summary
- dashboard_stats — O: card grid + status_indicator + progress_bar + empty_state
- data_table — O: table (virtualized optional) + pagination + filter_panel + empty_state
- employee_form — O: page_header + form_field groups + permissions sub-section
- header — O: logo Link + global search (search_bar) + action_menu + notification_toast trigger
- item_form — O: form_field groups + file_upload + confirm_dialog
- layout_designer — O: palette (Accordion) + canvas + properties panel (form_field)
- page_header — O: breadcrumb_nav + title + actions (Button/IconButton)
- permission_matrix — O: data_table variant + toggle/checkbox + bulk actions
- pricing_form — O: form_field groups + stepper (optional) + confirm_dialog
- reservation_form — O: date_range_picker + form_field + confirm_dialog
- timeline_view — O: grid + items + tooltip; keyboard range selection
- Sidebar — O: nav tree (Accordion) + Link atoms + collapse/expand + badges
- AppShell — O: Header + Sidebar + Content slot + RightRail slot
- RightRail — O: panel + tabs (tab_navigation) + actions
- GlobalSearchResults — O: list of results (card/list item) + keyboard navigation
- InboxList — O: list + status_indicator + pagination
- ActivityStream — O: filter_panel + list + pagination + empty_state

#### Templates (slots and composing organisms)
- calendar_template — T: page_header + Sidebar (optional) + calendar_view in content
- dashboard_template — T: page_header + grid of dashboard_stats in content
- detail_view_template — T: page_header + RightRail + content area for organisms
- form_view_template — T: page_header + form organism + RightRail (optional)
- list_view_template — T: page_header + data_table + filter_panel + RightRail (optional)
- WizardTemplate — T: page_header + Stepper + slot for step organism
- SplitViewTemplate — T: list_view_template left + detail_view_template right + resizer
- TwoPaneTemplate — T: Sidebar left + content right + page_header
- FocusTemplate — T: centered content + minimal header; no Sidebar

#### Pages (composition recap)
- Dashboard — T: dashboard_template; O: dashboard_stats grid
- Reservations — T: SplitViewTemplate; O: data_table + RightRail reservation_form
- Inventory — T: TwoPaneTemplate; O: data_table + filter_panel
- People — T: TwoPaneTemplate; O: data_table/forms per tab
- Finance — T: list_view_template/detail_view_template; O: data_table + pricing_form
- Chat — T: FocusTemplate/custom; A/M: chat_input, chat_message, chat_messages_list
- Settings — T: TwoPaneTemplate; O: form organisms per section
- Portal Home — T: dashboard_template (client variant); O: cards
- Browse — T: list_view_template; O: data_table + filter_panel
- Book — T: WizardTemplate; O: reservation_form + payment step
- My bookings — T: list/detail templates; O: data_table + detail
- Account — T: form_view_template; O: account_form
- Admin/ERD/Dev — T: AppShell; O: tool-specific organisms

#### Mobile adaptation (no native app required)
- Strategy: Responsive Web App + optional PWA. All components must support small screens (≥320px) using tokens and breakpoints.
- Layout
  - AppShell: collapsible Sidebar (off-canvas on mobile), Header shrinks to icon-only actions
  - Templates: TwoPane/SplitView collapse into stacked views with tabs/segmented control
- Navigation
  - Use CommandPalette and a bottom sheet menu for key actions on mobile
  - Breadcrumb collapses to back button + overflow menu
- Inputs and targets
  - Minimum touch target 44x44dp; increase spacing tokens on compact density for touch
  - Replace hover with focus/pressed states; ensure keyboard and screen reader flows
- Data-heavy views
  - DataTable: switch to cards on XS; horizontal scroll with sticky first column on S/M
  - Calendar/Timeline: agenda/list mode on XS; pinch/zoom disabled; explicit controls
- Performance
  - Virtualize long lists; defer-heavy content with Skeletons; reduce motion for prefers-reduced-motion
- PWA (optional)
  - Installable manifest + service worker for offline cache of static assets; no separate native app

Acceptance checklist (add to each story as applicable)
- [ ] Responsive variants (xs/s/m/l) with layout changes defined above
- [ ] Keyboard + screen reader support on touch devices
- [ ] Visual regression for mobile breakpoints in Storybook
- [ ] Playwright flows include mobile viewport (e.g., iPhone 12) for primary pages


---

### Master Checklists and Definitions of Done (DoD)

#### DoD: Design Tokens and Foundations
- [ ] Single source of truth in Storybook “Design Tokens” with live examples
- [ ] Tokens cover light/dark and density (comfortable/compact)
- [ ] Motion tokens defined and referenced; respects prefers-reduced-motion
- [ ] Contrast AA/AAA passes for text and UI elements (documented)
- [ ] No hard-coded colors/spacings in components
- [ ] Theming switch demo + tests for both modes

#### DoD: Atoms
- [ ] Props standardized (size, variant, disabled) and documented
- [ ] States: default, hover, focus-visible, active/pressed, disabled, error (if applicable)
- [ ] A11y: proper role/aria, label association, focus ring visible, tab order
- [ ] Tokens only (spacing/typography/color/elevation)
- [ ] Storybook: controls + states + visual regression baseline
- [ ] Tests: unit (behavior) + a11y (axe) + snapshot
- [ ] Mobile: touch target ≥44x44, spacing scales with tokens

#### DoD: Molecules
- [ ] Composition uses atoms only; no ad-hoc styles
- [ ] Keyboard/focus management (roving focus, escape, trap if overlay)
- [ ] Async/Loading/Skeleton states defined
- [ ] Empty/Error states where relevant
- [ ] Storybook: interactive stories incl. edge cases
- [ ] Tests: unit + property-based variants + a11y
- [ ] Mobile: layout adapts (stacking/collapse) and large targets

#### DoD: Organisms
- [ ] Composed of atoms/molecules; no page-specific logic
- [ ] Performance: virtualization for long lists where relevant
- [ ] States: loading, empty, error, no-permission
- [ ] Slots/props for all external data and actions (no global coupling)
- [ ] Tests: integration-level + a11y + visual regression
- [ ] Mobile: responsive mode (cards for tables, agenda for calendars)

#### DoD: Templates
- [ ] Page-agnostic; all content via slots/props
- [ ] Standard props: title, actions, toolbar, tabs, breadcrumbs, sidebar content
- [ ] Defined mobile behavior (collapse/stack/tabs)
- [ ] Storybook template demos for typical compositions
- [ ] Tests: layout smoke + responsive snapshots

#### DoD: Pages
- [ ] Built only from templates + organisms
- [ ] Playwright: auth, navigation, primary actions, error-free console
- [ ] Mobile viewport Playwright run included
- [ ] No page-unique components introduced

---

### Generic Checklists by Level

#### Atom Checklist (apply to each atom)
- [ ] Props documented (types, defaults, variants)
- [ ] States covered (default/hover/focus/pressed/disabled/error)
- [ ] A11y verified (labels/roles/tabindex/focus-visible)
- [ ] Token usage only; no hard-coded values
- [ ] Storybook states + controls + visual baseline
- [ ] Unit tests + axe checks
- [ ] Mobile touch target + spacing tokens

#### Molecule Checklist (apply to each molecule)
- [ ] Composition diagram (which atoms)
- [ ] Keyboard/focus flow defined and tested
- [ ] Loading/empty/error states defined
- [ ] Storybook with interactive scenarios
- [ ] Unit + property-based tests (variants, sizes)
- [ ] Axe checks pass
- [ ] Mobile stacked layout or adaptive pattern

#### Organism Checklist (apply to each organism)
- [ ] Slots/props only; no global state assumptions
- [ ] Performance plan (virtualize/defer)
- [ ] Loading/empty/error/no-permission states
- [ ] Storybook scenarios reflect real data shapes
- [ ] Integration tests + visual regression
- [ ] Responsive behavior documented and tested

#### Template Checklist (apply to each template)
- [ ] Standard props supported (title/actions/toolbar/tabs/breadcrumbs/sidebar)
- [ ] Works with/without Sidebar/RightRail
- [ ] Defined mobile collapse/stack rules
- [ ] Storybook examples with multiple organisms
- [ ] Snapshot tests at key breakpoints

#### Page Checklist (apply to each route)
- [ ] Composes approved templates/organisms only
- [ ] Primary user flows scripted in Playwright (incl. mobile viewport)
- [ ] Axe checks pass with no serious violations
- [ ] No console errors; network failures handled

---

### Priority Component Checklists (high impact)

#### Sidebar (Organism)
- [ ] Composition: Accordion (sections) + Link atoms + BadgeCounter + collapse/expand
- [ ] A11y: aria-current, focus management, keyboard expand/collapse
- [ ] Responsive: off-canvas on mobile; swipe/close with ESC and overlay click
- [ ] Active trail and nested groups visual states
- [ ] Tokens: spacing scale for dense/comfortable
- [ ] Storybook: wide/narrow/mobile states; RTL check
- [ ] Tests: keyboard nav, a11y, visual regression

#### AppShell (Organism)
- [ ] Composition: Header + Sidebar + Content slot + RightRail slot
- [ ] Layout tokens define gaps, z-index layers, elevations
- [ ] Responsive: Sidebar off-canvas, Header compact toolbar on mobile
- [ ] Skip-to-content link; landmark roles (header/nav/main/aside)
- [ ] Storybook: permutations (sidebar open/closed, with/without RightRail)
- [ ] Tests: layout smoke, keyboard skip, visual regression at breakpoints

#### CommandPalette (Molecule)
- [ ] Global shortcut (Cmd/Ctrl+K) with announcement for screen readers
- [ ] ARIA combobox/listbox pattern; typeahead; results navigation
- [ ] Debounced async search; loading/skeleton and empty state
- [ ] Recent/featured commands section; keyboard to execute/close
- [ ] Storybook: large dataset performance demo
- [ ] Tests: unit (filtering), a11y, property-based for command sets

#### DataTable (Organism)
- [ ] Column resizing, sorting, filtering, pagination
- [ ] Virtualization for large datasets; sticky header
- [ ] Empty/Error/Loading states; row selection with keyboard
- [ ] Mobile: card layout on XS; horizontal scroll w/ sticky first column on S/M
- [ ] Storybook: dense/comfortable, many columns, async data
- [ ] Tests: integration, a11y, visual regression, property-based for columns/rows

---

### Accessibility Checklist (apply repo-wide)
- [ ] Landmarks present; headings hierarchical
- [ ] Focus order logical; no focus traps except deliberate
- [ ] Focus-visible styles; sufficient contrast for all states
- [ ] Labels and aria-attributes correct; live regions polite
- [ ] Keyboard parity for all interactions (no mouse-only)
- [ ] Axe checks integrated into Storybook/Playwright CI

### Performance Checklist
- [ ] Virtualize long lists/tables; windowing thresholds defined
- [ ] Code-split heavy organisms/templates
- [ ] Defer non-critical content; use Skeletons
- [ ] Avoid layout thrashing; use transform for animations
- [ ] Measure with Web Vitals in dev; budgets tracked in CI

### Mobile/Responsive Checklist
- [ ] Breakpoints documented; each story includes xs/s/m/l snaps
- [ ] Hit target ≥44x44; spacing scales via tokens
- [ ] Replace hover with focus/pressed states; no hover-only affordances
- [ ] Tables → cards; two-pane → stacked/tabs; calendars → agenda
- [ ] Playwright mobile viewport runs for primary pages

### Storybook Coverage Checklist
- [ ] All atoms/molecules/organisms/templates have stories
- [ ] Stories include states and error/empty/loading where applicable
- [ ] Controls for props/variants; docs tab with usage examples
- [ ] Visual regression baselines established per story

### Testing Matrix Checklist
- [ ] Unit tests for atoms/molecules
- [ ] Integration tests for organisms/templates
- [ ] Property-based tests for combinations (sizes/variants/density)
- [ ] Playwright navigation flows per page (desktop + mobile viewport)
- [ ] CI gates on mix compile, mix credo, tests, visual diffs, axe

### Documentation and Governance Checklist
- [ ] Update docs/.airules with atomic rules and review checklist
- [ ] Update docs/styleguide.md with tokens, responsiveness, a11y rules
- [ ] Link relevant Storybook stories from docs
- [ ] Add “Design Review” checklist integrated with credo custom checks


---

### SaladUI Integration Plan (deterministic, AI-executable)

Rationale and approach
- Keep a stable, canonical UI API under `RivaAshWeb.Components.UI.*`; implement it by delegating to SaladUI primitives.
- Wrapping ensures: stable API, tokens/a11y enforcement, TwMerge class merging, future vendor swap safety.

Strict execution checklist (no phases/dates)
1) Preflight and configuration
   - [ ] Ensure dependency and build setup
     - [ ] Confirm `{:salad_ui, "~> 1.0.0-beta"}` exists in `packages/riva_ash/mix.exs`
     - [ ] Confirm `tw_merge` is present (transitive via SaladUI)
     - [ ] Confirm Tailwind content includes `../deps/salad_ui/lib/**/*.ex` in `assets/tailwind.config.js`
     - [ ] Confirm `assets/css/app.css` and `assets/css/storybook.css` import `salad_ui.css`
   - [ ] Guardrails
     - [ ] Update docs/ui-guidelines.md to mandate `RivaAshWeb.Components.UI.*` in new code
     - [ ] Add migration note: Atoms.* → UI.* (UI wraps SaladUI)

2) Implement canonical wrappers (apply pattern to each primitive)
   - Pattern
     - [ ] Keep module/function names (e.g., `UI.Button.button/1`)
     - [ ] Map assigns: `variant`, `size`, `disabled`, `class`, `rest` → SaladUI assigns
     - [ ] Merge classes with `TwMerge.merge([...])` to combine defaults + overrides
     - [ ] Preserve extra features (e.g., Button `loading` spinner) by composing around SaladUI component
     - [ ] Forward ARIA/global attributes; ensure focus-visible states
   - Components
     - [ ] Button → SaladUI.Button (variants: default, secondary, destructive, outline, ghost, link; sizes: default, sm, lg, icon)
     - [ ] Input → SaladUI.Input
     - [ ] Checkbox → SaladUI.Checkbox
     - [ ] Select → SaladUI.Select
     - [ ] Textarea → SaladUI.Textarea
     - [ ] Badge → SaladUI.Badge
     - [ ] Alert → SaladUI.Alert
     - [ ] Separator (new) → SaladUI.Separator
     - [ ] Skeleton (new) → SaladUI.Skeleton
     - [ ] Switch/Toggle (align API) → SaladUI.Switch or SaladUI.Toggle
   - API normalization
     - [ ] Align `variant` values with SaladUI; add compatibility mapping for legacy values
     - [ ] Align `size` values to `default|sm|lg|icon`; map legacy sizes if needed
     - [ ] Remove hard-coded hex/px; ensure tokens-only styling

3) Storybook
   - [ ] For each wrapped primitive, add/refresh stories covering:
     - [ ] All variants and sizes
     - [ ] States: default, hover, focus-visible, disabled, destructive
     - [ ] Dark/light (storybook parameters)
     - [ ] Axe checks enabled with documented rule exceptions
     - [ ] Visual regression baselines
   - [ ] Add stories for: Separator, Skeleton, Switch/Toggle

4) Migration of Atoms.* and call sites
   - [ ] Update Atoms.* to delegate to `UI.*`; mark deprecated in moduledoc
   - [ ] Search repo for Atoms.* and direct Tailwind markup in components
   - [ ] Replace usages with `UI.*` in small, targeted edits (component-by-component)
   - [ ] Optionally add dev-only `Logger.warning` or telemetry on Atoms.* entrypoints to surface usage

5) Adoption in Molecules/Organisms/Templates
   - [ ] Replace direct Tailwind utility class compositions with `UI.*` primitives wherever possible
   - [ ] DataTable: keep Flop/FlopPhoenix; may style with SaladUI.Table, but avoid duplicate logic
   - [ ] Rework `packages/riva_ash/components/ui/*.exs` (AppShell, Sidebar, CommandPalette) to compose `UI.*`/SaladUI and move into lib once stable

6) A11y, tokens, density
   - [ ] Verify all primitives use tokens and expose accessible roles/labels
   - [ ] Add shared focus-visible and aria helpers if gaps remain
   - [ ] Add `density: comfortable|compact` where relevant, wired to tokens for spacing

7) Testing (apply per component)
   - [ ] Unit tests: rendering, assigns mapping, ARIA/global attrs forwarding
   - [ ] Property-based tests: variants × sizes × states
   - [ ] Snapshot tests: Storybook or LiveView rendered output
   - [ ] Axe a11y checks: Storybook CI and phoenix_test flows

8) CI/lint/governance
   - [ ] Ensure CI runs compile, credo, tests, Storybook build, axe checks, visual diffs
   - [ ] Credo/custom rule discouraging new Atoms.* usage (suggest `UI.*`)
   - [ ] Script to fail CI if raw hex/px found in UI modules (enforce tokens)

Acceptance criteria (per component)
- [ ] Delegates to the correct SaladUI module and maps assigns correctly
- [ ] Uses `TwMerge.merge` to safely handle `class` overrides
- [ ] Uses tokens only; no hard-coded values
- [ ] Stories cover variants/sizes/states incl. dark/light; axe passes
- [ ] Unit + property-based tests pass; visual baseline established

Notes
- Direct SaladUI usage is allowed inside the `UI` layer for advanced composition; downstream application code should prefer `RivaAshWeb.Components.UI.*` to maintain consistency and vendor independence.
