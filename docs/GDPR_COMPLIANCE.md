# GDPR Compliance (Condensed)

Summary of current status and concrete actions. See prior version for detailed narrative.

## Status
- Strong: audit trails (AshPaperTrail), soft deletes (AshArchival), data minimization
- Partial: privacy by design, lawfulness (consent for non-essential processing)
- Needs work: consent mgmt, data subject rights automation, retention automation, cross-border, breach notification

## Roles & data
- Users (owners): controllers of org data
- Employees: processors for owners; data subjects for their personal data
- Clients: data subjects

## Technical safeguards
- Policies for tenant isolation and role-based access
- HTTPS/TLS, DB encryption, bcrypt; signed+encrypted session cookies
- Full diff audit logs; failed auth logging; access monitoring

## Data categories (with bases & retention)
- Owner: name, email, org → contract (6(1)(b)); lifetime + 7y
- Staff: name, email, phone, role → legitimate interest (6(1)(f)); employment + 3y
- Client: name, optional email/phone, reservations → contract (6(1)(b)); last interaction + 2y
- System: audit/session/metrics → legitimate interest (6(1)(f)); logs 1y, sessions 30d

## Gaps → actions
1) Consent management (30d)
- Add ConsentRecord resource (user_id, purpose, consent_given, consent_date, version, ip, user_agent)

2) Data subject rights (45d)
- Add actions: export_personal_data, request_data_deletion, rectify_personal_data

3) Retention automation (60d)
- Background job to delete expired logs, anonymize inactive clients, remove expired sessions

4) Cross-border controls (90d)
- Track residency and transfer consent; restrict processing by region when needed

5) Breach notifications (120d)
- Detect, classify, notify within 72h; runbooks + templates

6) Dynamic privacy policy (180d)
- Generate/privacy dashboard; reflect enabled processing purposes

## Implementation phases
- 0–60d: Consent, subject rights, export, retention jobs
- 60–120d: Breach notify, transfer controls
- 120–180d: Dynamic policies, DPIA tooling, anonymization, reporting

## Ops cadence
- Monthly: update ROPA, audit access, backups, deps
- Quarterly: DPIA for new features, retention review, breach drills, training
- Annual: full audit, policy refresh, third-party review

## Contacts
- DPO: TBA
- Tech lead: Architecture Team
- Legal: TBA
- privacy@rivaash.com
