defmodule RivaAsh.Validations.PaymentDataValidation do
  @moduledoc """
  Custom validation for payment data integrity.

  This validation module ensures that payment data meets all business rules
  and constraints before being processed or saved.
  """

  use Ash.Resource.Validation

  @impl true
  def validate(changeset, _opts) do
    amount_due = Ash.Changeset.get_attribute(changeset, :amount_due)
    amount_paid = Ash.Changeset.get_attribute(changeset, :amount_paid)
    currency = Ash.Changeset.get_attribute(changeset, :currency)

    cond do
      Decimal.compare(amount_due, Decimal.new(0)) == :lt ->
        {:error, "Amount due must be greater than or equal to 0"}

      not is_nil(amount_paid) and Decimal.compare(amount_paid, Decimal.new(0)) == :lt ->
        {:error, "Amount paid must be greater than or equal to 0"}

      not String.match?(currency, ~r/^[A-Z]{3}$/) ->
        {:error, "Currency must be a valid 3-letter ISO code"}

      true ->
        :ok
    end
  end

  @impl true
  def describe(_opts) do
    "Validates payment data integrity including amounts and currency format"
  end
end
