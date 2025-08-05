# This component has been deprecated and removed.
# Business card functionality is now handled by the ReservationCard component in storybook/combinations/

# TODO: Remove this file once all references have been migrated
# @deprecated Use ReservationCard component instead
defmodule RivaAshWeb.Components.Business.BusinessCard do
  @moduledoc """
  @deprecated This module is deprecated and will be removed.
  Business card functionality has been moved to the ReservationCard component.
  """

  use Phoenix.Component
  require Logger

  @doc """
  @deprecated Use ReservationCard component instead
  """
  @spec render(map()) :: Phoenix.LiveView.Rendered.t()
  def render(assigns) do
    Logger.warning("BusinessCard component is deprecated. Use ReservationCard instead.")

    assigns = Map.put_new(assigns, :_deprecated, true)

    ~H"""
    <div class="text-red-500 bg-red-50 p-4 border border-red-200 rounded">
      <p class="font-medium">Deprecated Component</p>
      <p class="text-sm">Business card functionality has been moved to ReservationCard component.</p>
    </div>
    """
  end
end
