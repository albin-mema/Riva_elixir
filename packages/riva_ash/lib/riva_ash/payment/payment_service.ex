defmodule RivaAsh.Payment.PaymentService do
  @moduledoc """
  Service module for managing Payments business logic.
  This module encapsulates all business logic related to payments,
  keeping it separate from the LiveView UI concerns.
  """

  require Logger

  alias RivaAsh.Repo
  alias RivaAsh.Resources.Payment
  alias RivaAsh.Resources.Business
  alias Ash.Query

  @doc """
  Get all payments for the current user with pagination.
  """
  def get_user_payments(user) do
    query =
      Payment
      |> Query.filter(business_id: user.business_id)
      |> Query.sort(inserted_at: :desc)
      |> Query.load([:business])

    try do
      case Ash.read(query, page: [limit: 20, offset: 0]) do
        {:ok, payments} ->
          # Get total count for pagination
          count_query =
            Payment
            |> Query.filter(business_id: user.business_id)
            |> Query.aggregate(:count, :id)

          case Ash.read(count_query) do
            {:ok, [%{count: total_count}]} ->
              meta = %{
                total_count: total_count,
                page_size: 20,
                current_page: 1,
                total_pages: ceil(total_count / 20)
              }

              {:ok, {payments, meta}}

            {:error, reason} ->
              Logger.error("Failed to count payments: #{inspect(reason)}")
              {:error, reason}
          end

        {:error, reason} ->
          Logger.error("Failed to get user payments: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_user_payments: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Create a new payment.
  """
  def create_payment(attrs, user) do
    try do
      case Ash.create(Payment,
             attributes:
               Map.merge(attrs, %{
                 business_id: user.business_id,
                 status: :pending
               }),
             authorize?: true
           ) do
        {:ok, payment} ->
          {:ok, payment}

        {:error, reason} ->
          Logger.error("Failed to create payment: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in create_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Update an existing payment.
  """
  def update_payment(id, attrs, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            case Ash.update(payment, attributes: attrs, authorize?: true) do
              {:ok, updated_payment} ->
                {:ok, updated_payment}

              {:error, reason} ->
                Logger.error("Failed to update payment: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment for update: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in update_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Delete a payment.
  """
  def delete_payment(id, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            case Ash.destroy(payment, authorize?: true) do
              {:ok, deleted_payment} ->
                {:ok, deleted_payment}

              {:error, reason} ->
                Logger.error("Failed to delete payment: #{inspect(reason)}")
                {:error, reason}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment for deletion: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in delete_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Process a payment.
  """
  def process_payment(id, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            if payment.status == :pending do
              # Here you would integrate with a payment processor
              # For now, we'll just update the status to completed
              case Ash.update(payment, attributes: %{status: :completed}, authorize?: true) do
                {:ok, updated_payment} ->
                  {:ok, updated_payment}

                {:error, reason} ->
                  Logger.error("Failed to process payment: #{inspect(reason)}")
                  {:error, reason}
              end
            else
              {:error, :payment_not_pending}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment for processing: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in process_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Cancel a payment.
  """
  def cancel_payment(id, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            if payment.status == :pending do
              case Ash.update(payment, attributes: %{status: :cancelled}, authorize?: true) do
                {:ok, updated_payment} ->
                  {:ok, updated_payment}

                {:error, reason} ->
                  Logger.error("Failed to cancel payment: #{inspect(reason)}")
                  {:error, reason}
              end
            else
              {:error, :payment_not_pending}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment for cancellation: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in cancel_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Refund a payment.
  """
  def refund_payment(id, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            if payment.status == :completed do
              case Ash.update(payment, attributes: %{status: :refunded}, authorize?: true) do
                {:ok, updated_payment} ->
                  {:ok, updated_payment}

                {:error, reason} ->
                  Logger.error("Failed to refund payment: #{inspect(reason)}")
                  {:error, reason}
              end
            else
              {:error, :payment_not_completed}
            end
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment for refund: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in refund_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get a single payment by ID.
  """
  def get_payment(id, user) do
    try do
      case Ash.get(Payment, id, authorize?: true) do
        {:ok, payment} ->
          if payment.business_id == user.business_id do
            {:ok, payment}
          else
            {:error, :forbidden}
          end

        {:error, reason} ->
          Logger.error("Failed to get payment: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_payment: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Validate payment attributes.
  """
  def validate_payment_attrs(attrs) do
    attrs
    |> validate_required_field(:client_id, "Client ID is required")
    |> validate_amount()
    |> validate_required_field(:currency, "Currency is required")
    |> validate_status()
    |> case do
      [] -> {:ok, attrs}
      errors -> {:error, errors}
    end
  end

  # Private helper functions for validation
  defp validate_required_field(attrs, field, error_message) do
    case Map.get(attrs, field) do
      value when is_binary(value) and byte_size(value) > 0 ->
        []

      _ ->
        [error_message]
    end
  end

  defp validate_amount(attrs) do
    case Map.get(attrs, :amount) do
      amount when is_number(amount) and amount > 0 ->
        attrs

      amount when is_number(amount) ->
        attrs ++ ["Amount must be greater than 0"]

      _ ->
        attrs ++ ["Amount is required and must be a positive number"]
    end
  end

  defp validate_currency(attrs) do
    case Map.get(attrs, :currency) do
      currency when is_binary(currency) and byte_size(currency) > 0 ->
        attrs

      _ ->
        attrs ++ ["Currency is required"]
    end
  end

  defp validate_status(attrs) do
    case Map.get(attrs, :status) do
      status when status in [:pending, :completed, :failed, :refunded, :cancelled] ->
        attrs

      _ ->
        attrs ++ ["Invalid status"]
    end
  end

  @doc """
  Check if payment amount is within allowed limits.
  """
  def within_amount_limits?(amount) do
    min_amount = Application.get_env(:riva_ash, :min_payment_amount, 0.01)
    max_amount = Application.get_env(:riva_ash, :max_payment_amount, 10_000.00)

    amount >= min_amount and amount <= max_amount
  end

  @doc """
  Get payment statistics for a business.
  """
  def get_payment_stats(business_id) do
    # Get total count
    total_count_query =
      Payment
      |> Query.filter(business_id: business_id)
      |> Query.aggregate(:count, :id)

    # Get total amount
    total_amount_query =
      Payment
      |> Query.filter(business_id: business_id, status: :completed)
      |> Query.aggregate(:sum, :amount)

    # Get completed count
    completed_count_query =
      Payment
      |> Query.filter(business_id: business_id, status: :completed)
      |> Query.aggregate(:count, :id)

    # Get pending count
    pending_count_query =
      Payment
      |> Query.filter(business_id: business_id, status: :pending)
      |> Query.aggregate(:count, :id)

    # Get failed count
    failed_count_query =
      Payment
      |> Query.filter(business_id: business_id, status: :failed)
      |> Query.aggregate(:count, :id)

    try do
      with {:ok, [%{count: total_count}]} <- Ash.read(total_count_query),
           {:ok, [%{sum: total_amount}]} <- Ash.read(total_amount_query),
           {:ok, [%{count: completed_count}]} <- Ash.read(completed_count_query),
           {:ok, [%{count: pending_count}]} <- Ash.read(pending_count_query),
           {:ok, [%{count: failed_count}]} <- Ash.read(failed_count_query) do
        stats = %{
          total_count: total_count,
          total_amount: total_amount || 0,
          completed_count: completed_count,
          pending_count: pending_count,
          failed_count: failed_count,
          success_rate: if(total_count > 0, do: completed_count / total_count * 100, else: 0)
        }

        {:ok, stats}
      else
        {:error, reason} ->
          Logger.error("Failed to get payment stats: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_payment_stats: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Get payment history for a client.
  """
  def get_client_payments(client_id, business_id) do
    query =
      Payment
      |> Query.filter(business_id: business_id, client_id: client_id)
      |> Query.sort(inserted_at: :desc)
      |> Query.load([:business])

    try do
      case Ash.read(query) do
        {:ok, payments} ->
          {:ok, payments}

        {:error, reason} ->
          Logger.error("Failed to get client payments: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Unexpected error in get_client_payments: #{inspect(error)}")
        {:error, :unexpected_error}
    end
  end

  @doc """
  Check if a payment can be processed.
  """
  def can_process_payment?(payment) do
    payment.status == :pending
  end

  @doc """
  Check if a payment can be cancelled.
  """
  def can_cancel_payment?(payment) do
    payment.status == :pending
  end

  @doc """
  Check if a payment can be refunded.
  """
  def can_refund_payment?(payment) do
    payment.status == :completed
  end
end
