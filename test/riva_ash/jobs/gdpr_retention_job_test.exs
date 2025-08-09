defmodule RivaAsh.Jobs.GDPRRetentionJobTest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureLog

  alias RivaAsh.Jobs.GDPRRetentionJob
  alias RivaAsh.GDPR.RetentionPolicy

  setup do
    # Start the job for each test
    {:ok, _} = GDPRRetentionJob.start_link(schedule: "* * * * *")
    :ok
  end

  describe "job execution" do
    test "runs retention cleanup successfully" do
      # Mock successful retention cleanup
      expect_retention_success()

      # Trigger immediate run
      :ok = GDPRRetentionJob.run_now()

      # Verify stats were updated
      stats = GDPRRetentionJob.get_stats()
      assert stats.execution_time_ms >= 0
      assert stats.report != nil
    end

    test "handles retention failure gracefully" do
      # Mock failed retention cleanup
      expect_retention_failure()

      # Trigger immediate run
      :ok = GDPRRetentionJob.run_now()

      # Verify error was logged
      log = capture_log(fn -> GDPRRetentionJob.run_now() end)
      assert log =~ "GDPR: Retention cleanup failed"
    end

    test "computes correct cutoff date" do
      # Mock current time for consistent testing
      current_time = DateTime.from_naive!(~N[2025-08-09 14:20:00], "Etc/UTC")
      cutoff = RetentionPolicy.calculate_retention_cutoff(current_time)

      # Verify cutoff is 2 years prior (example retention period)
      assert cutoff.year == 2023
      assert cutoff.month == 8
      assert cutoff.day == 9
    end

    test "uses proper Ash DB filters for retention" do
      # Mock current time
      current_time = DateTime.from_naive!(~N[2025-08-09 14:20:00], "Etc/UTC")

      # Get retention filters
      filters = RetentionPolicy.build_retention_filters(current_time)

      # Verify filter structure
      assert filters[:inserted_at][:lte] != nil
      assert filters[:archived_at][:lte] != nil
    end

    describe "telemetry events" do
      test "emits start event at beginning of execution" do
        :ok = GDPRRetentionJob.run_now()

        assert_receive {:telemetry_event, [:riva_ash, :gdpr_retention, :start], _measurements, _metadata}
        describe "integration: data retention" do
          setup do
            # Create test data older than retention period
            old_date = DateTime.add(DateTime.utc_now(), -3 * 365 * 24 * 3600, :second)

            # Create client with old data
            client = insert(:client, inserted_at: old_date)

            # Create reservation with old data
            reservation = insert(:reservation, inserted_at: old_date)

            %{client: client, reservation: reservation}
          end

          test "deletes old client data", %{client: client} do
            :ok = GDPRRetentionJob.run_now()
            assert Repo.get(RivaAsh.Resources.Client, client.id) == nil
          end

          test "anonymizes old reservation data", %{reservation: reservation} do
            :ok = GDPRRetentionJob.run_now()

            updated_reservation = Repo.get(RivaAsh.Resources.Reservation, reservation.id)
            assert updated_reservation.client_email == "anonymized@example.com"
            assert updated_reservation.client_phone == nil
          end

          test "preserves recent data" do
            # Create recent client
            recent_client = insert(:client)

            :ok = GDPRRetentionJob.run_now()
            assert Repo.get(RivaAsh.Resources.Client, recent_client.id) != nil
          end
        end
      end

      test "emits stop event with correct metrics after successful execution" do
        :ok = GDPRRetentionJob.run_now()

        assert_receive {:telemetry_event, [:riva_ash, :gdpr_retention, :stop], measurements, _metadata}
        assert measurements[:duration] >= 0
        assert measurements[:deleted_count] >= 0
        assert measurements[:anonymized_count] >= 0
      end

      test "emits exception event when retention fails" do
        # Force failure
        allow(RetentionPolicy.run_retention_cleanup(), return: {:error, "Test error"})

        :ok = GDPRRetentionJob.run_now()

        assert_receive {:telemetry_event, [:riva_ash, :gdpr_retention, :exception], _measurements, metadata}
        assert metadata[:reason] == "Test error"
      end
    end
  end

  describe "health monitoring" do
    test "reports healthy status when running normally" do
      # Mock successful run
      expect_retention_success()
      GDPRRetentionJob.run_now()

      # Verify health status
      {:ok, _} = GDPRRetentionJob.health_check()
    end

    test "reports warning when overdue" do
      # Mock last run as 26 hours ago
      last_run = DateTime.add(DateTime.utc_now(), -26 * 3600, :second)
      :sys.replace_state(GDPRRetentionJob, fn state -> %{state | last_run: last_run} end)

      # Verify health status
      {:warning, message} = GDPRRetentionJob.health_check()
      assert message =~ "overdue"
    end
  end

  describe "compliance reporting" do
    test "generates valid compliance report" do
      report = GDPRRetentionJob.generate_job_report()

      # Verify report structure
      assert report.job_name == "gdpr_retention_cleanup"
      assert report.schedule == "Daily at 2 AM UTC"
      assert is_map(report.last_run_stats)
      assert is_list(report.compliance_notes)
    end
  end

  # Helper functions
  defp expect_retention_success do
    # Mock successful retention cleanup
    allow(RetentionPolicy.run_retention_cleanup(), return: {:ok, %{deleted: 10, anonymized: 5}})
    allow(RetentionPolicy.generate_retention_report(), return: %{})
  end

  defp expect_retention_failure do
    # Mock failed retention cleanup
    allow(RetentionPolicy.run_retention_cleanup(), return: {:error, "Database error"})
  end
end
