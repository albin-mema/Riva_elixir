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
- [ ] Visual regression suite (percy or Storybook chromatic-equivalent if adopted)
- [ ] Accessibility: axe checks in Storybook and Playwright
- [ ] i18n and RTL snapshots for critical components
- [ ] Performance budgets for heavy organisms (DataTable, Calendar)

---

### Migration and Consistency
- [ ] Align legacy UI stories (packages/riva_ash/storybook/ui/*) with atoms/molecules structure
- [ ] Remove duplicate stories; keep canonical ones under atoms/molecules/organisms
- [ ] Ensure component API parity across similar components (Button vs IconButton)
- [ ] Centralize common behaviors (focus trap, portal, overlay) in utilities
- [ ] Document usage guidelines per component in docs/component_library_specifications.md

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

