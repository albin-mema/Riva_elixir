# Routes and UX Checklist

Use this checklist to validate the routes and navigation experience across web and mobile.

## Information Architecture
- [ ] Primary destinations limited to 5–7 and grouped logically
- [ ] Legacy routes mapped to new `/app/*` structure with 301 redirects
- [ ] Predictable, human-readable URLs; no trailing slashes
- [ ] Deep links for details and edit states

## Access Control
- [ ] Unauthenticated users redirected to `/auth/sign-in` with `return_to`
- [ ] Admin/dev routes gated by env and policy
- [ ] Error pages wired: `/404`, `/access-denied`, catch-all → 404

## Navigation & Layout
- [ ] Mobile bottom tab bar (Dashboard, Reservations, Inventory, People, Finance)
- [ ] Desktop top/side nav with breadcrumbs for nested pages
- [ ] Consistent action placement (FAB on mobile where relevant)
- [ ] Skeleton loading and empty states implemented

## URL State (LiveView & query params)
- [ ] Shareable state in URL via `live_patch`/`push_patch` and `handle_params`
- [ ] Tables use Flop params (`page[number]`, `page[size]`, `sort`, `order`, `filters[...]`)
- [ ] Filters/search encoded with compact, stable keys (e.g., `q`, `status`)
- [ ] Calendar views encoded via `view=list|calendar|month|week`, `date=YYYY-MM-DD`
- [ ] Multi-select filters use repeated keys or comma-separated values
- [ ] Large or ephemeral state kept out of URL

## Responsiveness
- [ ] One-column mobile layouts; multi-column desktop layouts with breakpoints
- [ ] Touch-friendly hit targets; avoid hover-only affordances

## Accessibility
- [ ] ARIA roles/labels for nav; keyboard navigable; focus visible
- [ ] Color contrast meets WCAG AA; reduced motion respected

## Internationalization
- [ ] All copy externalized; locale-aware dates/numbers
- [ ] Optional `?lang=xx` supported; future locale path support planned

## Performance
- [ ] Initial render within budget (e.g., <2s on slow 3G); interactions <150ms
- [ ] LiveView patching minimizes payload; avoid N+1 via Ash loading

## Observability
- [ ] Page_view analytics per `/app/*` route and key CTAs
- [ ] Error and latency logging include route names and context

## API & Docs
- [ ] OpenAPI at `/api/open_api` documents JSON:API and `/api/booking/*`
- [ ] Public endpoints rate limited; CORS configured appropriately

## Testing
- [ ] LiveView tests assert URL-driven state via `handle_params`
- [ ] Property-based tests for filters, pagination, and sorting
- [ ] Access/policy tests per route and scope

