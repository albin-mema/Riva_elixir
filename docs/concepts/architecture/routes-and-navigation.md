# Routes and Navigation Specification

Version: 1.0  
Audience: Product, Design, and Engineering (Elixir/Phoenix + LiveView)

## Goals
- Clarify a workflow-first IA suitable for both web and mobile
- Provide a predictable, stable URL structure with strong deep‑linking
- Keep legacy routes accessible with graceful redirects
- Document usage of URL query params for state in LiveView (answer: yes, when state is shareable)

## Summary of current routes (for context)
- Public: `/`, `/health`, `/docs`, `/search`, auth routes (`/sign-in`, `/register`, etc.)
- API: `/api` (Ash JSON:API, OpenAPI at `/api/open_api`), `/api/booking/*`
- Authenticated LiveViews: `/dashboard`, `/setup`, `/reservations`, `/inventory`, `/people`, `/finance`, `/chat`, `/settings` and legacy resource routes under top-level paths
- Admin/Tools: `/admin`, `/erd`, dev-only tools under `/dev/*`
- Errors: `/404`, `/access-denied`, catch-all → 404

## Proposed Information Architecture
Use a consistent app shell under `/app/*` for authenticated pages. Keep public, admin, api, and dev namespaces distinct.

- Public (unauthenticated)
  - `/` → redirects to `/app/dashboard` if signed in, otherwise `/auth/sign-in`
  - `/health`, `/docs`
  - Auth: `/auth/sign-in` (GET/LIVE + POST), `/auth/complete-sign-in`, `/auth/register` (GET/POST), `/auth/sign-out` (POST)

- App (authenticated)
  - Primary navigation (mobile bottom tabs; desktop top/side nav):
    - `/app/dashboard`
    - `/app/reservations`
    - `/app/inventory`
    - `/app/people`
    - `/app/finance`
  - Secondary destinations:
    - `/app/search`
    - `/app/settings`
    - `/app/chat`

- Admin and Tools
  - `/admin` (AshAdmin), `/admin/erd` (redirect `/erd` → `/admin/erd`)
  - `/dev/*` (dev only), Storybook stubs (dev/test)

- API
  - `/api` (JSON:API + OpenAPI at `/api/open_api`)
  - `/api/booking/*` (public booking endpoints)

- Errors
  - `/404`, `/access-denied`, catch-all → 404

## Routes by workflow (proposed)

- Dashboard
  - `GET/LIVE /app/dashboard`
  - `GET/LIVE /app/search`

- Reservations
  - `LIVE /app/reservations`
  - `LIVE /app/reservations/new`
  - `LIVE /app/reservations/:id`
  - `LIVE /app/reservations/:id/edit`
  - `LIVE /app/reservations/recurring`
  - `LIVE /app/reservations/recurring/:id`

- Inventory
  - `LIVE /app/inventory/items`
  - `LIVE /app/inventory/items/new`
  - `LIVE /app/inventory/items/:id`
  - `LIVE /app/inventory/items/:id/edit`
  - `LIVE /app/inventory/types`
  - `LIVE /app/inventory/layouts`
  - `LIVE /app/inventory/layouts/:id/edit`
  - `LIVE /app/inventory/sections`
  - `LIVE /app/inventory/plots`
  - `LIVE /app/inventory/positions`
  - `LIVE /app/inventory/schedules`
  - `LIVE /app/inventory/holds`
  - `LIVE /app/inventory/availability-exceptions`

- People
  - `LIVE /app/people/employees`
  - `LIVE /app/people/clients`
  - `LIVE /app/people/clients/new`
  - `LIVE /app/people/clients/:id/edit`
  - `LIVE /app/people/users`

- Finance
  - `LIVE /app/finance/payments`
  - `LIVE /app/finance/pricing`
  - `LIVE /app/finance/tokens`

- Setup & System
  - `LIVE /app/setup`
  - `LIVE /app/settings`
  - `LIVE /app/chat`

## Legacy route mapping (redirects)
Preserve old URLs with 301 redirects to `/app/*` equivalents.
- `/items` → `/app/inventory/items`
- `/items/new` → `/app/inventory/items/new`
- `/items/:id/edit` → `/app/inventory/items/:id/edit`
- `/clients` → `/app/people/clients`, etc. (apply similarly to all legacy resource routes)

## URL conventions
- Hyphenate words (e.g., `availability-exceptions`); no trailing slashes
- Use UUIDs for IDs; optional `:id-:slug` pattern for readability
- Prefer simple, stable query param keys; keep semantics consistent across pages
- Flop-compatible params for tables: `page[number]`, `page[size]`, `sort`, `order`, `filters[...]`
- Case-insensitive paths; canonical redirects (e.g., `/:id-:slug` → normalize slug)

## URL state and query parameters in LiveView (Elixir answer)
Short answer: Yes—storing shareable view state in the URL is a good practice in LiveView. LiveView supports URL-driven state via `live_patch/2`, `push_patch`, and `handle_params/3`. Use query params for state that should survive refresh, be shareable, and support back/forward navigation. Keep purely ephemeral UI state out of the URL.

Recommended patterns
- Prefer `live_patch/2` or `push_patch(socket, to: ~p"/path?key=val")` to update the URL without a full LV remount.
- In the LiveView, implement `handle_params(params, _uri, socket)` to derive assigns from the URL.
- Keep a single “source of truth”: assign values from params; when UI controls change, update the URL (patch) and let `handle_params` reconcile state.
- Use compact keys and consistent shapes across pages. Examples:
  - Tables: `?q=chair&page[number]=2&page[size]=20&sort=name&order=asc`
  - Filters: `?status=active,pending&type=standard`
  - Calendar: `?view=month&date=2025-08-01`
- For multi-select, use repeated keys (`?status=active&status=pending`) or comma‑separated values. Keep under practical URL length limits (~2–4 KB).
- Avoid encoding large blobs. If needed, serialize small, typed structures (e.g., `?range=2025-08-01..2025-08-31`).

Example sketch

```elixir
# in .heex
<.link patch={~p"/app/reservations?view=#{@view}&date=#{@date}"}>Month</.link>

# in LiveView
@impl true
def handle_params(params, _uri, socket) do
  view = params["view"] || "list"
  date = params["date"] || Date.utc_today() |> Date.to_iso8601()
  {:noreply, assign(socket, view: view, date: date)}
end

# when a control changes
def handle_event("set_view", %{"view" => view}, socket) do
  {:noreply, push_patch(socket, to: ~p"/app/reservations?view=#{view}&date=#{socket.assigns.date}")}
end
```

What belongs in the URL
- Filtering, sorting, pagination
- Selected date/range, active tab, view mode (list/calendar)
- Resource identifiers (current record, nested drill-ins)

What stays out of the URL
- Ephemeral UI state: transient toasts, focus, unsaved form buffer
- Large internal state (use assigns, JS hooks, or session as appropriate)

Tables and Flop
- Standardize on Flop param naming; provide helpers to convert UI filters to Flop maps and back to params.
- Keep param names stable to support sharable links and tests.

Testing
- Add LiveView tests that manipulate query params and assert state via `handle_params` pathways.

## Navigation patterns
- Mobile: bottom tab bar (Dashboard, Reservations, Inventory, People, Finance); overflow “More” for Settings, Chat, Search. FAB for primary actions.
- Desktop: top/side nav with breadcrumbs for nested details; two‑panel layouts for list/detail.

## Accessibility & i18n
- ARIA roles/labels on navigation; visible focus rings; keyboard navigation throughout
- All icon buttons have text alternatives; motion‑reduced animations
- Locale-aware dates/numbers; copy externalized; RTL-ready components

## Analytics & observability
- Name routes for metrics; track page_view per `/app/*` and key CTAs
- Log errors/latency with route names for easy dashboards

## Implementation notes (non‑binding)
- Add new `/app` scope with existing authenticated pipelines
- Move workflow LiveViews under `/app/*` paths; add 301 redirects from legacy routes
- Keep `/api` and `/admin` unchanged; redirect `/erd` → `/admin/erd`
- Ensure Flop params preserved via `live_patch` and `handle_params`

