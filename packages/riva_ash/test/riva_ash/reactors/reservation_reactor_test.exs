defmodule RivaAsh.Reactors.ReservationReactorTest do
  use RivaAsh.DataCase, async: true

  import RivaAsh.Test.MoxHelpers
  import RivaAsh.Test.TimeHelpers
  alias RivaAsh.Factory

  describe "success path" do
    @spec test_produces_reservation_and_payment :: :ok
    test "produces reservation and payment" do
      # TODO: set up input context via Factory, set Mox expectations for external calls
      # result = ReservationReactor.run(input)
      assert true
    end
  end

  describe "failure path" do
    @spec test_rolls_back_and_emits_error :: :ok
    test "rolls back and emits error" do
      # TODO: force validation error or external failure and assert rollback behavior
      assert true
    end
  end

  describe "idempotency" do
    @spec test_re_run_is_idempotent :: :ok
    test "re-run is idempotent" do
      # TODO: run the reactor twice with same input; assert no duplicates and consistent result
      assert true
    end
  end
end