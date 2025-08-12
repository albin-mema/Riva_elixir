defmodule RivaAsh.ReservationDecision do
  @moduledoc """
  Behaviour and adapter entry point for reservation validation decisions.

  We can switch implementations via config:
    config :riva_ash, :reservation_decision_impl, RivaAsh.ReservationDecision.ElixirImpl
  or
    RivaAsh.ReservationDecision.HaskellGrpcImpl
  """

  @type availability_status :: {:available, non_neg_integer()} | {:partial, non_neg_integer()} | {:unavailable, String.t()}

  @callback check_availability(String.t(), DateTime.t(), DateTime.t()) :: {:ok, availability_status} | {:error, term}

  def impl do
    Application.get_env(:riva_ash, :reservation_decision_impl, RivaAsh.ReservationDecision.ElixirImpl)
  end

  def check_availability(item_id, start_dt, end_dt) do
    impl().check_availability(item_id, start_dt, end_dt)
  end
end

