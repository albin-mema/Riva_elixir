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
  """

  use Ash.Resource,
    domain: RivaAsh.Domain,
    data_layer: AshPostgres.DataLayer,
    extensions: [
      AshJsonApi.Resource,
      AshGraphql.Resource,
      AshPaperTrail.Resource,
      AshArchival.Resource
    ]

  # Configure versioning for payment tracking
  paper_trail do
    change_tracking_mode(:full_diff)
    ignore_attributes([:inserted_at, :updated_at])
    store_action_name?(true)
    store_action_inputs?(true)
    store_resource_identifier?(true)
    create_version_on_destroy?(true)
  end

  postgres do
    table("payments")
    repo(RivaAsh.Repo)
  end

  # Configure soft delete functionality
  archive do
    attribute(:archived_at)
    base_filter?(false)
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

      change(set_attribute(:status, :paid))

      validate(present(:amount_paid), message: "Amount paid is required")
      validate(compare(:amount_paid, greater_than: 0), message: "Amount paid must be greater than 0")
    end

    # Action to process refund
    update :process_refund do
      accept([:refund_amount, :refund_reason, :refund_date])

      change(set_attribute(:status, :refunded))

      validate(present(:refund_amount), message: "Refund amount is required")
      validate(compare(:refund_amount, greater_than: 0), message: "Refund amount must be greater than 0")
      validate(compare(:refund_amount, less_than_or_equal_to: :amount_paid),
        message: "Refund amount cannot exceed amount paid")
    end
  end

  attributes do
    uuid_primary_key(:id)

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

    # TODO: Add conditional validations for payment_date and refund_reason
    # These would need custom validation functions
  end

  calculations do
    # Calculate remaining balance
    calculate :balance_remaining, :decimal, expr(
      amount_due - coalesce(amount_paid, 0)
    ) do
      public?(true)
      description("Remaining balance to be paid")
    end

    # Check if fully paid
    calculate :is_fully_paid, :boolean, expr(
      status == :paid and amount_paid >= amount_due
    ) do
      public?(true)
      description("Whether the payment is fully paid")
    end
  end
end
