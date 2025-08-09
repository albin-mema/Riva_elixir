defmodule RivaAsh.Reactors.ReservationReactorTest do
  use RivaAsh.DataCase, async: true

  import RivaAsh.Test.MoxHelpers
  import RivaAsh.Test.TimeHelpers
  alias RivaAsh.Factory

  describe "success path" do
    test "produces reservation and payment" do
      # TODO: set up input context via Factory, set Mox expectations for external calls
      # result = ReservationReactor.run(input)
      assert true
    end
  end

  describe "failure path" do
    test "rolls back and emits error" do
      # TODO: force validation error or external failure and assert rollback behavior
      assert true
    end
  end

  describe "idempotency" do
    test "re-run is idempotent" do
      # TODO: run the reactor twice with same input; assert no duplicates and consistent result
      assert true
    end
  end
end
