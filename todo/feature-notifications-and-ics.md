# Feature: Notifications and ICS

Goal: Send booking emails (confirmation, changes, cancellation) with ICS attachments in business timezone.

Tasks
- [ ] Email templates: confirmation, change, cancellation, reminder (24h)
- [ ] ICS generation: DTSTART/DTEND in business tz; UID=reservation id; METHOD=REQUEST/CANCEL
- [ ] Mailer integration (dev + prod adapters)
- [ ] Trigger points: after create/confirm, after cancel_by_staff, reminder job

Validation/Acceptance
- [ ] Emails contain correct local dates/times (DST-safe)
- [ ] ICS imports correctly into Google/Apple/Outlook
- [ ] Reminder job respects storm closures and status changes

Tests
- [ ] Unit: ICS builder around DST edge days
- [ ] Integration: create/cancel; assert emails and ICS content

