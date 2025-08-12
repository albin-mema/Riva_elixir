defmodule RivaAsh.Validations.ReservationDecisionAdapter do
  @moduledoc """
  Thin adapter to use the configured ReservationDecision implementation in validations.
  """

  alias RivaAsh.ReservationDecision

  @spec check_availability(String.t(), DateTime.t(), DateTime.t()) :: {:ok, {:available, non_neg_integer()} | {:partial, non_neg_integer()} | {:unavailable, String.t()}} | {:error, term}
  def check_availability(item_id, start_dt, end_dt) do
    ReservationDecision.check_availability(item_id, start_dt, end_dt)
  end
end

