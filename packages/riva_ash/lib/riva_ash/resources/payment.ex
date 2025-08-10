defmodule RivaAsh.Resources.Payment do
  @moduledoc """
  Represents payment information for reservations.
  Tracks payment status, method, and amounts for full-day reservations.

  Payment methods:
  - cash: Cash payment (most common)
  - card: Credit/debit card payment
  - bank_transfer: Bank transfer
  - other: Other payment methods

  Payment status:
  - pending: Payment not yet received
  - paid: Payment completed
  - partial: Partial payment received
  - refunded: Payment refunded
  - cancelled: Payment cancelled

  This resource manages payment processing, tracking, and refund handling
  for reservations, ensuring proper financial record keeping.
  """

  @type t :: %__MODULE__{
          id: String.t(),
          business_id: String.t(),
          reservation_id: String.t(),
          pricing_id: String.t() | nil,
          status: :pending | :paid | :partial | :refunded | :cancelled,
          amount_due: Decimal.t(),
          amount_paid: Decimal.t() | nil,
          currency: String.t(),
          payment_method: :cash | :card | :bank_transfer | :other,
          payment_date: DateTime.t() | nil,
          due_date: Date.t() | nil,
          transaction_reference: String.t() | nil,
          refund_amount: Decimal.t() | nil,
          refund_date: DateTime.t() | nil,
          refund_reason: String.t() | nil,
          notes: String.t() | nil,
          inserted_at: DateTime.t(),
          updated_at: DateTime.t(),
          archived_at: DateTime.t() | nil
        }

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    authorizers: [Ash.Policy.Authorizer],
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  import RivaAsh.ResourceHelpers
  import RivaAsh.Authorization

  postgres do
    table("payments")
    repo(RivaAsh.Repo)
  end

  standard_archive()
  standard_paper_trail()

  # Authorization policies
  policies do
    # Admins can do everything
    bypass actor_attribute_equals(:role, :admin) do
      authorize_if(always())
    end

    # Business owners can manage payments for their business (optimized with denormalized business_id)
    policy action_type([:read, :create, :update, :destroy]) do
      authorize_if(expr(business.owner_id == ^actor(:id)))
    end

    # Employees can manage payments within their business (optimized)
    policy action_type([:read, :create, :update]) do
      authorize_if(action_has_permission(:manage_payments))
    end

    # Clients can view their own payments
    policy action_type(:read) do
      authorize_if(expr(reservation.client_id == ^actor(:id)))
    end
  end

  json_api do
    type("payment")

    routes do
      base("/payments")

      get(:read)
      index(:read)
      post(:create)
      patch(:update)
      delete(:destroy)

      # Additional routes for payment specific actions
      get(:by_reservation, route: "/by-reservation/:reservation_id")
      get(:by_client, route: "/by-client/:client_id")
      get(:by_business, route: "/by-business/:business_id")
      get(:pending, route: "/pending")
      get(:paid, route: "/paid")
      get(:overdue, route: "/overdue")
    end
  end

  graphql do
    type(:payment)

    queries do
      get(:get_payment, :read)
      list(:list_payments, :read)
      list(:payments_by_reservation, :by_reservation)
      list(:payments_by_client, :by_client)
      list(:payments_by_business, :by_business)
      list(:pending_payments, :pending)
      list(:paid_payments, :paid)
      list(:overdue_payments, :overdue)
    end

    mutations do
      create(:create_payment, :create)
      update(:update_payment, :update)
      update(:mark_as_paid, :mark_as_paid)
      update(:process_refund, :process_refund)
      destroy(:delete_payment, :destroy)
    end
  end

  code_interface do
    define(:create, action: :create)
    define(:read, action: :read)
    define(:update, action: :update)
    define(:destroy, action: :destroy)
    define(:by_id, args: [:id], action: :by_id)
    define(:by_reservation, args: [:reservation_id], action: :by_reservation)
    define(:by_client, args: [:client_id], action: :by_client)
    define(:by_business, args: [:business_id], action: :by_business)
    define(:pending, action: :pending)
    define(:paid, action: :paid)
    define(:overdue, action: :overdue)
    define(:mark_as_paid, action: :mark_as_paid)
    define(:process_refund, action: :process_refund)
    define(:process_payment, action: :process_payment)
    define(:cancel_payment, action: :cancel_payment)
    define(:for_user_business, args: [:business_id], action: :for_user_business)
    define(:search_payments, args: [:search_term, :business_id], action: :search_payments)
  end

  actions do
    defaults([:read, :update, :destroy])

    create :create do
      accept([
        :reservation_id,
        :pricing_id,
        :amount_due,
        :currency,
        :payment_method,
        :due_date,
        :notes
      ])

      primary?(true)

      # Set initial status to pending
      change(set_attribute(:status, :pending))

      # Automatically set business_id from reservation
      change(&RivaAsh.Changes.set_business_id_from_reservation/2)

      # Validate cross-business relationships
      validate(&RivaAsh.Validations.validate_reservation_payment_business_match/2)
    end

    read :by_id do
      argument(:id, :uuid, allow_nil?: false)
      get?(true)
      filter(expr(id == ^arg(:id)))
    end

    read :by_reservation do
      argument(:reservation_id, :uuid, allow_nil?: false)
      filter(expr(reservation_id == ^arg(:reservation_id)))
    end

    read :by_client do
      argument(:client_id, :uuid, allow_nil?: false)
      filter(expr(reservation.client_id == ^arg(:client_id)))
    end

    read :by_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(reservation.item.section.business_id == ^arg(:business_id)))
    end

    read :pending do
      filter(expr(status == :pending))
    end

    read :paid do
      filter(expr(status == :paid))
    end

    read :overdue do
      filter(expr(status == :pending and due_date < fragment("CURRENT_DATE")))
    end

    # Action to mark payment as paid
    update :mark_as_paid do
      accept([:amount_paid, :payment_method, :payment_date, :transaction_reference, :notes])
      require_atomic?(false)

      change(set_attribute(:status, :paid))

      validate(present(:amount_paid), message: "Amount paid is required")

      validate(compare(:amount_paid, greater_than: 0),
        message: "Amount paid must be greater than 0"
      )
    end

    # Action to process refund
    update :process_refund do
      accept([:refund_amount, :refund_reason, :refund_date])
      require_atomic?(false)

      change(set_attribute(:status, :refunded))

      validate(present(:refund_amount), message: "Refund amount is required")

      validate(compare(:refund_amount, greater_than: 0),
        message: "Refund amount must be greater than 0"
      )

      validate(compare(:refund_amount, less_than_or_equal_to: :amount_paid),
        message: "Refund amount cannot exceed amount paid"
      )
    end

    # Process payment (from PaymentService)
    update :process_payment do
      accept([:amount_paid, :payment_method, :payment_date, :transaction_reference])
      require_atomic?(false)

      # Only allow processing pending payments
      validate(fn changeset, _context ->
        if Ash.Changeset.get_data(changeset, :status) == :pending do
          :ok
        else
          {:error, field: :status, message: "Only pending payments can be processed"}
        end
      end)

      change(set_attribute(:status, :paid))
      change(set_attribute(:payment_date, &DateTime.utc_now/0))

      validate(present(:amount_paid), message: "Amount paid is required")
      validate(compare(:amount_paid, greater_than: 0), message: "Amount paid must be greater than 0")
    end

    # Cancel payment (from PaymentService)
    update :cancel_payment do
      accept([:notes])
      require_atomic?(false)

      # Only allow cancelling pending payments
      validate(fn changeset, _context ->
        if Ash.Changeset.get_data(changeset, :status) == :pending do
          :ok
        else
          {:error, field: :status, message: "Only pending payments can be cancelled"}
        end
      end)

      change(set_attribute(:status, :cancelled))
    end

    # Get user payments with pagination (from PaymentService)
    read :for_user_business do
      argument(:business_id, :uuid, allow_nil?: false)
      filter(expr(business_id == ^arg(:business_id)))
      prepare(build(load: [:business, :reservation], sort: [inserted_at: :desc]))
    end

    # Search payments (from PaymentService)
    read :search_payments do
      argument(:search_term, :string, allow_nil?: false)
      argument(:business_id, :uuid, allow_nil?: false)

      filter(expr(business_id == ^arg(:business_id)))

      filter(
        expr(
          contains(transaction_reference, ^arg(:search_term)) or
            contains(notes, ^arg(:search_term))
        )
      )

      prepare(build(load: [:business, :reservation]))
    end
  end

  attributes do
    uuid_primary_key(:id)

    attribute :business_id, :uuid do
      allow_nil?(false)
      public?(true)
      description("Denormalized business ID for performance optimization")
    end

    attribute :status, :atom do
      constraints(one_of: [:pending, :paid, :partial, :refunded, :cancelled])
      default(:pending)
      public?(true)
      description("Current payment status")
    end

    attribute :amount_due, :decimal do
      allow_nil?(false)
      public?(true)
      constraints(min: 0)
      description("Total amount due for the reservation")
    end

    attribute :amount_paid, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Amount actually paid")
    end

    attribute :currency, :string do
      allow_nil?(false)
      default("USD")
      public?(true)
      constraints(max_length: 3)
      description("Currency code (ISO 4217)")
    end

    attribute :payment_method, :atom do
      constraints(one_of: [:cash, :card, :bank_transfer, :other])
      default(:cash)
      public?(true)
      description("Method of payment")
    end

    attribute :payment_date, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the payment was received")
    end

    attribute :due_date, :date do
      allow_nil?(true)
      public?(true)
      description("When payment is due")
    end

    attribute :transaction_reference, :string do
      allow_nil?(true)
      public?(true)
      description("Reference number for the transaction")
    end

    attribute :refund_amount, :decimal do
      allow_nil?(true)
      public?(true)
      constraints(min: 0)
      description("Amount refunded (if any)")
    end

    attribute :refund_date, :utc_datetime do
      allow_nil?(true)
      public?(true)
      description("When the refund was processed")
    end

    attribute :refund_reason, :string do
      allow_nil?(true)
      public?(true)
      description("Reason for the refund")
    end

    attribute :notes, :string do
      allow_nil?(true)
      public?(true)
      description("Additional notes about the payment")
    end

    create_timestamp(:inserted_at)
    update_timestamp(:updated_at)
  end

  relationships do
    belongs_to :business, RivaAsh.Resources.Business do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("Denormalized business relationship for performance")
    end

    belongs_to :reservation, RivaAsh.Resources.Reservation do
      allow_nil?(false)
      attribute_writable?(true)
      public?(true)
      description("The reservation this payment is for")
    end

    belongs_to :pricing, RivaAsh.Resources.Pricing do
      allow_nil?(true)
      attribute_writable?(true)
      public?(true)
      description("The pricing rule used for this payment")
    end
  end

  validations do
    validate(compare(:amount_due, greater_than: 0),
      message: "Amount due must be greater than 0"
    )

    validate(compare(:amount_paid, less_than_or_equal_to: :amount_due),
      message: "Amount paid cannot exceed amount due"
    )

    validate(compare(:refund_amount, less_than_or_equal_to: :amount_paid),
      message: "Refund amount cannot exceed amount paid"
    )

    validate(match(:currency, ~r/^[A-Z]{3}$/),
      message: "Currency must be a valid 3-letter ISO code"
    )

    # Conditional validation for payment_date
    validate(
      fn changeset, _context ->
        status = Ash.Changeset.get_attribute(changeset, :status)
        payment_date = Ash.Changeset.get_attribute(changeset, :payment_date)

        if status in [:paid, :partially_paid] && is_nil(payment_date) do
          {:error, field: :payment_date, message: "Payment date is required when payment is marked as paid"}
        else
          :ok
        end
      end,
      message: "Payment date is required when status is paid or partially_paid"
    )

    # Conditional validation for refund_reason
    validate(
      fn changeset, _context ->
        refund_amount = Ash.Changeset.get_attribute(changeset, :refund_amount)
        refund_reason = Ash.Changeset.get_attribute(changeset, :refund_reason)

        if refund_amount && Decimal.gt?(refund_amount, 0) &&
             (is_nil(refund_reason) || refund_reason == "") do
          {:error, field: :refund_reason, message: "Refund reason is required when refund amount is specified"}
        else
          :ok
        end
      end,
      message: "Refund reason is required when refund amount is greater than 0"
    )

    # Validation to ensure payment_date is not in the future
    validate(
      fn changeset, _context ->
        payment_date = Ash.Changeset.get_attribute(changeset, :payment_date)

        if payment_date && Timex.compare(payment_date, Timex.today()) == :gt do
          {:error, field: :payment_date, message: "Payment date cannot be in the future"}
        else
          :ok
        end
      end,
      message: "Payment date cannot be in the future"
    )

    # Validation to ensure refund amount doesn't exceed paid amount
    validate(
      fn changeset, _context ->
        amount_paid = Ash.Changeset.get_attribute(changeset, :amount_paid)
        refund_amount = Ash.Changeset.get_attribute(changeset, :refund_amount)

        if amount_paid && refund_amount && Decimal.gt?(refund_amount, amount_paid) do
          {:error, field: :refund_amount, message: "Refund amount cannot exceed amount paid"}
        else
          :ok
        end
      end,
      message: "Refund amount cannot exceed the amount paid"
    )
  end

  calculations do
    # Calculate remaining balance
    calculate :balance_remaining, :decimal, expr(amount_due - coalesce(amount_paid, 0)) do
      public?(true)
      description("Remaining balance to be paid")
    end

    # Check if fully paid
    calculate :is_fully_paid, :boolean, expr(status == :paid and amount_paid >= amount_due) do
      public?(true)
      description("Whether the payment is fully paid")
    end

    # Helper functions for business logic and data validation

    @doc """
    Checks if the payment is currently active (not archived).

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if the payment is active, `false` otherwise
    """
    @spec is_active?(t()) :: boolean()
    def is_active?(payment) do
      case payment do
        %{archived_at: nil} -> true
        _ -> false
      end
    end

    @doc """
    Checks if the payment is fully paid.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if fully paid, `false` otherwise
    """
    @spec fully_paid?(t()) :: boolean()
    def fully_paid?(payment) do
      case payment do
        %{status: :paid, amount_paid: amount_paid} when not is_nil(amount_paid) ->
          Decimal.compare(amount_paid, payment.amount_due) != :lt

        _ ->
          false
      end
    end

    @doc """
    Checks if the payment is pending.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if pending, `false` otherwise
    """
    @spec pending?(t()) :: boolean()
    def pending?(payment), do: payment.status == :pending

    @doc """
    Checks if the payment is overdue.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if overdue, `false` otherwise
    """
    @spec overdue?(t()) :: boolean()
    def overdue?(payment) do
      case payment do
        %{status: :pending, due_date: due_date} when not is_nil(due_date) ->
          Date.compare(due_date, Date.utc_today()) == :lt

        _ ->
          false
      end
    end

    @doc """
    Checks if the payment has been refunded.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if refunded, `false` otherwise
    """
    @spec refunded?(t()) :: boolean()
    def refunded?(payment), do: payment.status == :refunded

    @doc """
    Gets the payment status as a human-readable string.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with status description
    """
    @spec status_description(t()) :: String.t()
    def status_description(payment) do
      case payment.status do
        :pending -> "Pending"
        :paid -> "Paid"
        :partial -> "Partially Paid"
        :refunded -> "Refunded"
        :cancelled -> "Cancelled"
        _ -> "Unknown"
      end
    end

    @doc """
    Gets the payment method as a human-readable string.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with payment method description
    """
    @spec payment_method_description(t()) :: String.t()
    def payment_method_description(payment) do
      case payment.payment_method do
        :cash -> "Cash"
        :card -> "Card"
        :bank_transfer -> "Bank Transfer"
        :other -> "Other"
        _ -> "Unknown"
      end
    end

    @doc """
    Gets the remaining balance to be paid.

    ## Parameters
    - payment: The payment record

    ## Returns
    - Decimal with remaining balance
    """
    @spec remaining_balance(t()) :: Decimal.t()
    def remaining_balance(payment) do
      case payment.amount_paid do
        nil -> payment.amount_due
        amount_paid -> Decimal.max(Decimal.new(0), Decimal.sub(payment.amount_due, amount_paid))
      end
    end

    @doc """
    Gets the formatted amount due.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with formatted amount due
    """
    @spec formatted_amount_due(t()) :: String.t()
    def formatted_amount_due(payment) do
      formatted_currency(payment.amount_due, payment.currency)
    end

    @doc """
    Gets the formatted amount paid.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with formatted amount paid or "Not paid"
    """
    @spec formatted_amount_paid(t()) :: String.t()
    def formatted_amount_paid(payment) do
      case payment.amount_paid do
        nil -> "Not paid"
        amount -> formatted_currency(amount, payment.currency)
      end
    end

    @doc """
    Gets the formatted remaining balance.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with formatted remaining balance
    """
    @spec formatted_remaining_balance(t()) :: String.t()
    def formatted_remaining_balance(payment) do
      remaining_balance(payment) |> formatted_currency(payment.currency)
    end

    @doc """
    Gets the formatted due date.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with formatted due date or "No due date"
    """
    @spec formatted_due_date(t()) :: String.t()
    def formatted_due_date(payment) do
      case payment.due_date do
        nil -> "No due date"
        date -> Date.to_string(date)
      end
    end

    @doc """
    Gets the formatted payment date.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with formatted payment date or "Not paid"
    """
    @spec formatted_payment_date(t()) :: String.t()
    def formatted_payment_date(payment) do
      case payment.payment_date do
        nil -> "Not paid"
        date -> DateTime.to_string(date)
      end
    end

    @doc """
    Validates that the payment has all required relationships.

    ## Parameters
    - payment: The payment record to validate

    ## Returns
    - `{:ok, payment}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_relationships(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_relationships(payment) do
      cond do
        is_nil(payment.business) ->
          {:error, "Business relationship is missing"}

        is_nil(payment.reservation) ->
          {:error, "Reservation relationship is missing"}

        true ->
          {:ok, payment}
      end
    end

    @doc """
    Gets the reservation information for this payment.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with reservation information
    """
    @spec reservation_info(t()) :: String.t()
    def reservation_info(payment) do
      case payment.reservation do
        %{id: id, reserved_from: start_date, reserved_until: end_date} ->
          "Reservation #{id}: #{Date.to_string(start_date)} - #{Date.to_string(end_date)}"

        _ ->
          "Unknown reservation"
      end
    end

    @doc """
    Gets the business name for this payment.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with business name
    """
    @spec business_name(t()) :: String.t()
    def business_name(payment) do
      case payment.business do
        %{name: name} when is_binary(name) and name != "" -> name
        _ -> "Unknown business"
      end
    end

    @doc """
    Formats the complete payment information for display.

    ## Parameters
    - payment: The payment record

    ## Returns
    - String with complete payment information
    """
    @spec formatted_info(t()) :: String.t()
    def formatted_info(payment) do
      case is_active?(payment) do
        true ->
          business_name = business_name(payment)
          reservation_info = reservation_info(payment)
          status_desc = status_description(payment)
          amount_due = formatted_amount_due(payment)
          amount_paid = formatted_amount_paid(payment)
          remaining_balance = formatted_remaining_balance(payment)
          due_date = formatted_due_date(payment)
          payment_method = payment_method_description(payment)

          "#{business_name} - #{reservation_info}: #{status_desc}, Due: #{amount_due}, Paid: #{amount_paid}, Balance: #{remaining_balance}, Due: #{due_date}, Method: #{payment_method}"

        false ->
          "Archived payment: #{reservation_info(payment)}"
      end
    end

    @doc """
    Checks if the payment can be marked as paid.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if can be marked as paid, `false` otherwise
    """
    @spec can_mark_as_paid?(t()) :: boolean()
    def can_mark_as_paid?(payment) do
      payment.status == :pending and Decimal.compare(payment.amount_due, Decimal.new(0)) == :gt
    end

    @doc """
    Checks if the payment can be refunded.

    ## Parameters
    - payment: The payment record to check

    ## Returns
    - `true` if can be refunded, `false` otherwise
    """
    @spec can_refund?(t()) :: boolean()
    def can_refund?(payment) do
      case payment.status do
        :paid -> true
        :partial -> true
        _ -> false
      end
    end

    @doc """
    Gets the maximum refund amount.

    ## Parameters
    - payment: The payment record

    ## Returns
    - Decimal with maximum refund amount
    """
    @spec max_refund_amount(t()) :: Decimal.t()
    def max_refund_amount(payment) do
      case payment.amount_paid do
        nil -> Decimal.new(0)
        amount_paid -> amount_paid
      end
    end

    @doc """
    Validates the payment data.

    ## Parameters
    - payment: The payment record to validate

    ## Returns
    - `{:ok, payment}` if valid
    - `{:error, reason}` if invalid
    """
    @spec validate_data(t()) :: {:ok, t()} | {:error, String.t()}
    def validate_data(payment) do
      cond do
        Decimal.compare(payment.amount_due, Decimal.new(0)) == :lt ->
          {:error, "Amount due must be greater than or equal to 0"}

        not is_nil(payment.amount_paid) and Decimal.compare(payment.amount_paid, Decimal.new(0)) == :lt ->
          {:error, "Amount paid must be greater than or equal to 0"}

        not String.match?(payment.currency, ~r/^[A-Z]{3}$/) ->
          {:error, "Currency must be a valid 3-letter ISO code"}

        true ->
          {:ok, payment}
      end
    end

    # Private helper functions

    defp formatted_currency(amount, currency) do
      "#{Decimal.to_string(amount)} #{currency}"
    end
  end
end
