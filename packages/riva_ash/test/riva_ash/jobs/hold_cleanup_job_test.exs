defmodule RivaAsh.Jobs.HoldCleanupJobTest do
  use RivaAsh.DataCase, async: false

  import RivaAsh.Test.TimeHelpers
  import Ash.Expr
  alias RivaAsh.Resources.ItemHold
  alias RivaAsh.Domain
  alias RivaAsh.Jobs.HoldCleanupJob

  defp insert_hold!(attrs) do
    ItemHold
    |> Ash.Changeset.for_create(:create, attrs)
    |> Ash.create!(domain: Domain)
  end

  describe "removes expired holds" do
    test "deactivates holds past expiration and keeps valid ones" do
      base_time = ~U[2025-01-01 00:00:00Z]

      with_frozen_time(base_time, fn ->
        # Non-expired: expires in +5 minutes
        _valid =
          insert_hold!(%{
            is_active: true,
            expires_at: DateTime.add(base_time, 300, :second)
          })

        # Expired: expired 5 minutes ago
        expired =
          insert_hold!(%{
            is_active: true,
            expires_at: DateTime.add(base_time, -300, :second)
          })

        # Run cleanup
        {:ok, _pid} = start_supervised(HoldCleanupJob)
        # Trigger immediate cleanup
        count = GenServer.call(HoldCleanupJob, :cleanup_now, 5_000)
        assert count == 1

        # Assert expired is now inactive, valid unchanged
        {:ok, expired_after} =
          ItemHold
          |> Ash.get(expired.id, domain: Domain)

        assert expired_after.is_active == false

        {:ok, valid_count} =
          ItemHold
          |> Ash.Query.filter(expr(is_active == true))
          |> Ash.count(domain: Domain)

        assert valid_count == 1
      end)
    end
  end

  describe "concurrent runs safe (idempotent)" do
    test "multiple runs don't over-deactivate or error" do
      base_time = ~U[2025-01-01 00:00:00Z]

      with_frozen_time(base_time, fn ->
        expired =
          insert_hold!(%{
            is_active: true,
            expires_at: DateTime.add(base_time, -1, :second)
          })

        {:ok, _pid} = start_supervised(HoldCleanupJob)
        c1 = GenServer.call(HoldCleanupJob, :cleanup_now, 5_000)
        c2 = GenServer.call(HoldCleanupJob, :cleanup_now, 5_000)

        assert c1 in [0, 1]
        assert c2 in [0, 1]

        {:ok, exp_after} = ItemHold |> Ash.get(expired.id, domain: Domain)
        assert exp_after.is_active == false

        {:ok, active_count} =
          ItemHold
          |> Ash.Query.filter(expr(is_active == true))
          |> Ash.count(domain: Domain)

        assert active_count == 0
      end)
    end
  end
end
