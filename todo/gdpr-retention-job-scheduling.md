# GDPR retention job scheduling

Goal: Ensure GDPR retention job (RivaAsh.Jobs.GdprRetentionJob) runs on a reliable cadence in non-test envs and is covered by tests.

- [ ] Confirm job module exists and behaviour
  - [ ] Inspect lib/riva_ash/jobs/gdpr_retention_job.ex for run frequency and safety

- [ ] Ensure it is supervised/started
  - [ ] Check Application children include the job or a scheduler
  - [ ] If not, add Oban/Quantum or lightweight Process.send_after loop with jitter
  - [ ] Gate by env config; disabled in :test

- [ ] Add tests
  - [ ] Unit: job computes correct cutoff and filters with proper Ash DB filters
  - [ ] Integration: simulate passage of time and assert deletions/archivals

- [ ] Observability
  - [ ] Telemetry events and logs at start/end with counts and durations

- [ ] Docs
  - [ ] docs/GDPR_IMPLEMENTATION_CHECKLIST.md: mark scheduler configured

