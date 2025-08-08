defmodule RivaAsh.Changes do
  @moduledoc """
  Custom change functions for Ash resources.
  These functions handle automatic data transformations and denormalization for performance.
  """

  # This module contains utility functions for changes, not a single change implementation

  @doc """
  Automatically sets business_id from the related item for performance optimization.
  This denormalizes the business_id to avoid deep joins in queries.
  """
  @spec set_business_id_from_item(Ash.Changeset.t(), keyword()) :: Ash.Changeset.t()
  def set_business_id_from_item(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :item_id) do
      nil ->
        changeset

      item_id when is_binary(item_id) ->
        apply_business_id_from_item(changeset, item_id)
    end
  end

  @spec apply_business_id_from_item(Ash.Changeset.t(), String.t()) :: Ash.Changeset.t()
  defp apply_business_id_from_item(changeset, item_id) do
    case Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain) do
      {:ok, item} ->
        Ash.Changeset.force_change_attribute(changeset, :business_id, item.business_id)

      {:error, _unmatched} ->
        # Item not found - let validation handle this error
        changeset
    end
  end

  @doc """
  Automatically sets business_id from the related client for performance optimization.
  """
  @spec set_business_id_from_client(Ash.Changeset.t(), keyword()) :: Ash.Changeset.t()
  def set_business_id_from_client(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :client_id) do
      nil ->
        changeset

      client_id when is_binary(client_id) ->
        apply_business_id_from_client(changeset, client_id)
    end
  end

  @spec apply_business_id_from_client(Ash.Changeset.t(), String.t()) :: Ash.Changeset.t()
  defp apply_business_id_from_client(changeset, client_id) do
    case Ash.get(RivaAsh.Resources.Client, client_id, domain: RivaAsh.Domain) do
      {:ok, client} ->
        Ash.Changeset.force_change_attribute(changeset, :business_id, client.business_id)

      {:error, _unmatched} ->
        # Client not found - let validation handle this error
        changeset
    end
  end

  @doc """
  Automatically sets business_id from the related employee for performance optimization.
  """
  @spec set_business_id_from_employee(Ash.Changeset.t(), keyword()) :: Ash.Changeset.t()
  def set_business_id_from_employee(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :employee_id) do
      nil ->
        changeset

      employee_id when is_binary(employee_id) ->
        apply_business_id_from_employee(changeset, employee_id)
    end
  end

  @spec apply_business_id_from_employee(Ash.Changeset.t(), String.t()) :: Ash.Changeset.t()
  defp apply_business_id_from_employee(changeset, employee_id) do
    case Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain) do
      {:ok, employee} ->
        Ash.Changeset.force_change_attribute(changeset, :business_id, employee.business_id)

      {:error, _unmatched} ->
        # Employee not found - let validation handle this error
        changeset
    end
  end

  @doc """
  Automatically sets business_id from the related reservation for performance optimization.
  """
  @spec set_business_id_from_reservation(Ash.Changeset.t(), keyword()) :: Ash.Changeset.t()
  def set_business_id_from_reservation(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :reservation_id) do
      nil ->
        changeset

      reservation_id when is_binary(reservation_id) ->
        apply_business_id_from_reservation(changeset, reservation_id)
    end
  end

  @spec apply_business_id_from_reservation(Ash.Changeset.t(), String.t()) :: Ash.Changeset.t()
  defp apply_business_id_from_reservation(changeset, reservation_id) do
    case Ash.get(RivaAsh.Resources.Reservation, reservation_id, domain: RivaAsh.Domain) do
      {:ok, reservation} ->
        Ash.Changeset.force_change_attribute(changeset, :business_id, reservation.business_id)

      {:error, _unmatched} ->
        # Reservation not found - let validation handle this error
        changeset
    end
  end
end
