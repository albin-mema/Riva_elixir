# Chat TODO (Dev-first)

A simple, usable plan for development. We’ll tighten rules later.

## Status (done)
- [x] ChatRoom requires business_id; room_type: "internal" | "client_support"
- [x] ChatMessage -> belongs_to :room; supports sender_user and sender_client
- [x] ChatParticipant join resource (user OR client) with uniqueness per room
- [x] Minimal LiveView flow (internal): send messages, broadcast, render sender
- [x] DB migrations applied (rooms, messages, participants; sender columns)

## Next small steps (dev)
- [ ] Add ClientSupportLive (public entry) with minimal identification form (name/email)
  - [ ] find_or_create Client (by email) for the target business
  - [ ] reuse or create one client_support room for (business, client)
  - [ ] add participants: client + at least one business manager/owner (simple default)
  - [ ] broadcast/presence identical to internal rooms
- [ ] Presence hygiene
  - [ ] Track per-room on join; untrack on room switch
  - [ ] Replace generic "chat:general" subscription with per-room subscriptions only
- [ ] Room creation UX (internal)
  - [ ] Replace dev helper get_user_default_business/1 with a real lookup of user's businesses
  - [ ] On create, add creator as an owner participant (simple after_action or Live handling)
- [ ] Room listing
  - [ ] Show only rooms where the actor is a participant (both internal and client_support)

## Policies (later, keep simple first)
- [ ] ChatRoom
  - [ ] read: participants only; internal rooms require employee of the business; client_support allows client or employee
  - [ ] create: employees of business; client_support created by employee
  - [ ] update/delete: creator or business admins
- [ ] ChatParticipant
  - [ ] create: employees/managers only; can add clients (support) or employees (internal)
  - [ ] delete/read: align with room rules; destructive ops restricted to admins/owners
- [ ] ChatMessage
  - [ ] read: participants only
  - [ ] create: participants only; sender set from actor (user or client)

## Tests to add
- [ ] Resources
  - [ ] ChatParticipant: exactly-one-of (user_id, client_id), uniqueness per room, simple policy checks
  - [ ] ChatRoom: create/read/delete happy paths; basic policy checks
  - [ ] ChatMessage: create with sender_user/sender_client; deny when not participant
- [ ] LiveView
  - [ ] Internal: join, send, presence updates, switch rooms
  - [ ] ClientSupport: identify, join/reuse room, send, deny when not participant

## Decisions to confirm (can proceed with defaults)
- Default employee participant for client_support rooms: auto-add business owner/manager
- One room per (client, business) for client_support: yes
- Route for client support: `/support/:business_id` (can switch to slug later)

## Cleanup later
- Replace temporary get_user_default_business/1 helper with proper query
- Tweak UI components as needed (sender display, avatars, badges)
- Tighten GraphQL/JSON:API exposure once policies are enforced

