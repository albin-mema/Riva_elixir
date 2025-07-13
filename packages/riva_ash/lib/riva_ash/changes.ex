defmodule RivaAsh.Changes do
  @moduledoc """
  Custom change functions for Ash resources.
  These functions handle automatic data transformations and denormalization for performance.
  """

  import OK, only: [success: 1, failure: 1, ~>>: 2]

  # This module contains utility functions for changes, not a single change implementation

  @doc """
  Automatically sets business_id from the related item for performance optimization.
  This denormalizes the business_id to avoid deep joins in queries.
  """
  def set_business_id_from_item(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :item_id) do
      nil ->
        changeset
      item_id ->
        Ash.get(RivaAsh.Resources.Item, item_id, domain: RivaAsh.Domain)
        ~>> fn item ->
          Ash.Changeset.force_change_attribute(changeset, :business_id, item.business_id)
        end
        |> case do
          {:ok, updated_changeset} -> updated_changeset
          {:error, _} ->
            # Item not found - let validation handle this error
            changeset
        end
    end
  end

  @doc """
  Automatically sets business_id from the related client for performance optimization.
  """
  def set_business_id_from_client(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :client_id) do
      nil ->
        changeset
      client_id ->
        Ash.get(RivaAsh.Resources.Client, client_id, domain: RivaAsh.Domain)
        ~>> fn client ->
          Ash.Changeset.force_change_attribute(changeset, :business_id, client.business_id)
        end
        |> case do
          {:ok, updated_changeset} -> updated_changeset
          {:error, _} ->
            # Client not found - let validation handle this error
            changeset
        end
    end
  end

  @doc """
  Automatically sets business_id from the related employee for performance optimization.
  """
  def set_business_id_from_employee(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :employee_id) do
      nil ->
        changeset
      employee_id ->
        Ash.get(RivaAsh.Resources.Employee, employee_id, domain: RivaAsh.Domain)
        ~>> fn employee ->
          Ash.Changeset.force_change_attribute(changeset, :business_id, employee.business_id)
        end
        |> case do
          {:ok, updated_changeset} -> updated_changeset
          {:error, _} ->
            # Employee not found - let validation handle this error
            changeset
        end
    end
  end

  @doc """
  Automatically sets business_id from the related reservation for performance optimization.
  """
  def set_business_id_from_reservation(changeset, _opts) do
    case Ash.Changeset.get_argument_or_attribute(changeset, :reservation_id) do
      nil ->
        changeset
      reservation_id ->
        Ash.get(RivaAsh.Resources.Reservation, reservation_id, domain: RivaAsh.Domain)
        ~>> fn reservation ->
          Ash.Changeset.force_change_attribute(changeset, :business_id, reservation.business_id)
        end
        |> case do
          {:ok, updated_changeset} -> updated_changeset
          {:error, _} ->
            # Reservation not found - let validation handle this error
            changeset
        end
    end
  end
end
