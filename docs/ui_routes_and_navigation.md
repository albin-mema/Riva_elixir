## UI Routes and Navigation Map

Scope: Define all major pages (existing and proposed), how a user navigates between them, and where primary UI components (header, sidebar, content) are placed. Focus is on placement and positioning; not on visual styling.

Design references used for layout patterns: Linear, Notion, GitHub, Stripe Dashboard, Vercel, and Apple HIG/Material guidance for spacing and hierarchy.

---

### Global Layout (Desktop-first, responsive)
- Header (top, sticky):
  - Left: App logo/name (click → role-specific Home/Dashboard), global workspace switcher (if multi-tenant later)
  - Center: Global search bar (Cmd/Ctrl+K), inline quick actions
  - Right: Notifications bell, Help/Docs, User avatar menu (Profile, Account, Sign out)
- Sidebar (left, sticky, collapsible to icons at 80px; expanded ~264–288px):
  - Primary nav groups based on role (Client, Internal/Staff, Superadmin, DevTools)
  - Section labels with subtle separators
  - Active item highlighted; secondary level expands inline
- Content area:
  - Top: Breadcrumbs + page title + primary actions (right-aligned)
  - Main: Page content/workspace
  - Optional right rail (contextual panel) for details/editors when appropriate
- Mobile/Tablet:
  - Header remains; sidebar becomes a slide-over (hamburger in header-left)
  - Breadcrumbs collapse to back button; primary actions in an overflow menu

Refer to docs/breadcrumb_nav_spec.md for breadcrumb behavior; align spacing/tokens with styleguide.md and storybook design tokens.

---

### Role Overview
- Client (Public/Customer portal): lightweight booking and account pages
- Internal (Authenticated staff): operational console (dashboard, reservations, inventory, people, finance, chat, settings)
- Superadmin: AshAdmin and platform ops under /admin
- DevTools (dev-only): developer utilities under /dev

---

### Route Inventory and Placement

#### Public/Unauthenticated
- / → Redirects to role-appropriate home (if signed-in → /dashboard, else → /sign-in)
  - Entry points: Logo click, deep links
- /search (GlobalSearchLive)
  - Header: Full-width search prominent on this page
  - Sidebar: Hidden (focus page) or collapsed
- /sign-in, /register, /auth/complete-sign-in
  - Minimal layout (use browser_no_layout or simple header with logo)
  - Primary CTA centered; secondary link to Register/Sign in
- /docs → Swagger/OpenAPI viewer (public docs)
- /health → health check (no UI)

Proposed client portal (does not yet exist; UI complements existing booking APIs):
- /portal (Client Home)
  - Sidebar: Client nav (Home, Browse, My bookings, Account)
  - Content: Recent bookings, quick CTA to Browse availability
- /portal/browse (Availability browse/search)
  - Content: Filters top, results grid; select item → details
- /portal/book/:id (Booking flow)
  - Right rail or stepper for details → confirm → pay (if enabled)
- /portal/bookings (My bookings list)
  - Row actions: View, Reschedule (if policy), Cancel
- /portal/account (Profile & preferences)

Navigation between client pages:
- Header logo/name → /portal
- My bookings link from confirmation → /portal/bookings
- Account avatar click → /portal/account

#### Authenticated Internal (existing)
- /dashboard (DashboardLive)
  - Header: Search, Notifications, User menu
  - Sidebar (Internal): Dashboard, Reservations, Inventory, People, Finance, Chat, Settings
  - Content: KPIs, quick links to active work
- /setup (BusinessSetupLive)
  - Shown when onboarding incomplete; prominent CTA to configure essentials
- /reservations (ReservationCenterLive)
  - Content: Calendar/table views; right rail for details
- /inventory (InventoryManagementLive)
  - Sections: Items, Item Types, Layouts, Schedules, Positions, Holds
- /people (PeopleManagementLive)
  - Tabs: Clients, Employees, Users
- /finance (FinancialOperationsLive)
  - Subsections: Payments, Pricing (link through to resources below)
- /chat (ChatLive)
- /settings (SystemSettingsLive)

Legacy resource routes (kept for parity; accessible from Inventory/People/Finance subsections or via deep links):
- /businesses, /employees, /clients, /items, /item-types, /layouts, /item-schedules,
  /item-positions, /item-holds, /payments, /pricings, /plots, /recurring-reservations,
  /recurring-reservation-instances, /availability-exceptions, /sections, /users, /tokens
- Placement: Do not list all in primary sidebar. Surface as nested links inside the relevant parent page (Inventory, People, Finance), and via in-page tabs or a secondary sidebar.

Proposed additions (internal):
- /inbox (Notifications/Tasks center)
  - Header bell → opens panel; “View all” → /inbox
- /activity (Audit/activity stream)
  - From Settings and detail pages → Activity

Navigation between internal pages:
- Header logo/name → /dashboard
- Sidebar: primary section jumps
- Inside pages: breadcrumbs handle hierarchy (e.g., Inventory → Items → Edit)
- Right rail may contain links to related entities (e.g., Client → Bookings)

#### Superadmin
- /admin (AshAdmin)
  - Access: only for superadmins
  - Placement: Sidebar item “Admin” appears for superadmins
  - Layout: Keep standard header; AshAdmin within content area
- /erd (MermaidController)
  - Placement: Tools submenu for superadmins and developers ("System → ERD")

Navigation:
- Sidebar Admin item → /admin
- Tools → ERD → /erd

#### DevTools (dev-only)
- /dev (DevToolsHomeLive)
- /dev/ash-inspector
- /dev/business-context
- /dev/reactor-visualizer
- /dev/test-data-generator
- /dev/performance-dashboard

Placement:
- Sidebar: “Dev Tools” group visible only in dev
- Header: Optional quick “Dev” badge opens a launcher

Navigation:
- Dev Tools group → individual utilities
- Cross-links between tools (e.g., from Test Data Generator to Business Context)

---

### Sidebar Structure by Role (expanded state)

Client
- Home (/portal)
- Browse (/portal/browse)
- My bookings (/portal/bookings)
- Account (/portal/account)

Internal (Staff)
- Dashboard (/dashboard)
- Work
  - Reservations (/reservations)
  - Inventory (/inventory)
  - People (/people)
  - Finance (/finance)
- Communication
  - Chat (/chat)
  - Inbox (/inbox) [proposed]
- System
  - Settings (/settings)
  - Activity (/activity) [proposed]
  - ERD (/erd) [superadmin/dev only]
  - Admin (/admin) [superadmin only]

Dev (dev-env only)
- Dev Tools (/dev)
  - Ash Inspector (/dev/ash-inspector)
  - Business Context (/dev/business-context)
  - Reactor Visualizer (/dev/reactor-visualizer)
  - Test Data Generator (/dev/test-data-generator)
  - Performance Dashboard (/dev/performance-dashboard)

Notes
- Collapse behavior: Keep top-level icons visible; show tooltips on hover
- Active trail: highlight parent when child active

---

### Header Elements and Placement
- Left
  - App logo/name → role home
  - Sidebar toggle (hamburger) on small screens
- Center
  - Command palette/global search (Cmd/Ctrl+K)
  - Contextual quick actions (e.g., “New reservation”)
- Right
  - Notifications bell (badge count) → panel; link to /inbox
  - Help/Docs (opens docs/README.md or external)
  - User avatar → menu: Profile, Account, Settings, Sign out

Spacing and behavior
- Header height ~56–64px; sticky with subtle shadow
- Keep focus ring and accessible hit targets; keyboard shortcuts for search and sidebar

---

### Navigation Flows (examples)
- Onboarded staff: / → /dashboard → click “Reservations” → open reservation details (right rail) → “Client” link → People → Client detail
- New staff: /sign-in → /setup (until complete) → /dashboard
- Client: /portal/browse → select item → /portal/book/:id → confirm → link to /portal/bookings
- Superadmin: /dashboard → Admin → /admin → back via breadcrumbs
- Developer: /dev → pick tool → iterate; link to ERD when exploring relations

---

### Mapping to Current Router
Existing routes detected (high level):
- Public: /, /search, /sign-in, /register, /auth/complete-sign-in, /docs, /health
- Authenticated: /dashboard, /setup, /reservations, /inventory, /people, /finance, /chat, /settings
- Legacy resources: see list above
- Dev: /dev/* (dev-only)
- Admin: /admin/* (AshAdmin)
- ERD: /erd

Proposed (not yet implemented):
- Client portal: /portal, /portal/browse, /portal/book/:id, /portal/bookings, /portal/account
- Internal additions: /inbox, /activity

---

### Implementation Notes (positioning-first)
- Use authenticated layout pipeline for internal pages (already in router) with a consistent header and sidebar shell
- For focus-heavy pages (auth, booking flow), hide sidebar and expand content; keep header minimal
- Right rail: Use LiveView dynamic panel anchored to content area; slide-in on selection
- Breadcrumbs: derive from route segments and in-page context; keep within content header bar
- Accessibility: all nav elements keyboard reachable; visible focus; responsive collapse points at ~1024px and ~768px

---

### Next Steps
- Confirm proposed client portal and internal additions
- Align sidebar groups with product priorities; prune legacy items from primary nav
- Implement placeholder LiveViews for proposed routes (even minimal stubs) so flows can be tested
- Add Playwright flows to validate end-to-end navigation (per property-based testing preference)

