defmodule RivaAshWeb.Components.Forms.PaymentForm do
  @moduledoc """
  Payment processing form component.

  This component follows the functional core, imperative shell pattern,
  with pure functions for data transformation and validation, and
  the LiveView component handling UI state and side effects.

  ## Styleguide Compliance

  This module follows the Riva Ash styleguide principles:

  - **Functional Programming**: Uses pure functions, pattern matching, and pipelines
  - **Type Safety**: Comprehensive type specifications with @spec annotations
  - **Single Level of Abstraction**: Each function has a clear, focused responsibility
  - **Error Handling**: Consistent use of result tuples and guard clauses
  - **Immutable Data**: All transformations use immutable data structures
  - **Security**: Proper input validation and safe rendering patterns
  - **Phoenix/Ash Integration**: Follows Phoenix LiveView and Ash framework patterns
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button

  @type assigns :: %{
          optional(:form) => map(),
          optional(:reservation) => map(),
          optional(:payment_methods) => list(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:loading) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => any()
        }

  @type payment_form_data :: %{
          payment_method: String.t(),
          card_number: String.t(),
          card_holder_name: String.t(),
          expiry_month: String.t(),
          expiry_year: String.t(),
          cvv: String.t(),
          bank_account: String.t(),
          routing_number: String.t(),
          amount_received: float(),
          change_given: float(),
          notes: String.t()
        }

  @type reservation_summary :: %{
          client_name: String.t(),
          item_name: String.t(),
          date: String.t(),
          total_amount: float()
        }

  @doc """
  Renders a payment form.

  ## Examples
      <.payment_form
        form={@form}
        reservation={@reservation}
        payment_methods={@payment_methods}
        on_submit="process_payment"
        on_change="validate_payment"
        on_cancel="cancel_payment"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:reservation, :map, required: true)
  attr(:payment_methods, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec payment_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def payment_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_payment_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:payment_methods, [])
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_payment_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_payment_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.render_reservation_summary reservation={@reservation} />
      <.render_payment_method_selection form={@form} payment_methods={@payment_methods} />
      <.render_payment_method_fields form={@form} />
      <.render_notes_field form={@form} />
      <.render_form_actions loading={@loading} on_cancel={@on_cancel} />
    </form>
    """
  end

  @spec render_reservation_summary(reservation :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_reservation_summary(assigns) do
    ~H"""
    <div>
      <h3>Payment Details</h3>
      <div>
        <h4>Reservation Summary</h4>
        <p>Client: <%= @reservation.client_name %></p>
        <p>Item: <%= @reservation.item_name %></p>
        <p>Date: <%= @reservation.date %></p>
        <p>Amount: $<%= format_currency(@reservation.total_amount) %></p>
      </div>
    </div>
    """
  end

  @spec render_payment_method_selection(form :: map(), payment_methods :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_payment_method_selection(assigns) do
    ~H"""
    <.select_field
      field={@form[:payment_method]}
      label="Payment Method"
      options={payment_method_options(@payment_methods)}
      required
    />
    """
  end

  @spec render_payment_method_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_payment_method_fields(assigns) do
    ~H"""
    <div :if={show_credit_card_fields?(@form[:payment_method].value)}>
      <.render_credit_card_fields form={@form} />
    </div>
    <div :if={show_bank_transfer_fields?(@form[:payment_method].value)}>
      <.render_bank_transfer_fields form={@form} />
    </div>
    <div :if={show_cash_fields?(@form[:payment_method].value)}>
      <.render_cash_fields form={@form} />
    </div>
    """
  end

  @spec render_credit_card_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_credit_card_fields(assigns) do
    ~H"""
    <h4>Credit Card Information</h4>
    <.form_field field={@form[:card_number]} label="Card Number" type="text" required />
    <.form_field field={@form[:card_holder_name]} label="Cardholder Name" type="text" required />

    <div class="flex gap-4">
      <.select_field field={@form[:expiry_month]} label="Month" options={expiry_month_options()} required />
      <.select_field field={@form[:expiry_year]} label="Year" options={expiry_year_options()} required />
      <.form_field field={@form[:cvv]} label="CVV" type="text" required />
    </div>
    """
  end

  @spec render_bank_transfer_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_bank_transfer_fields(assigns) do
    ~H"""
    <h4>Bank Transfer Information</h4>
    <.form_field field={@form[:bank_account]} label="Bank Account" type="text" required />
    <.form_field field={@form[:routing_number]} label="Routing Number" type="text" required />
    """
  end

  @spec render_cash_fields(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_cash_fields(assigns) do
    ~H"""
    <h4>Cash Payment</h4>
    <.form_field field={@form[:amount_received]} label="Amount Received" type="number" step="0.01" required />
    <.form_field field={@form[:change_given]} label="Change Given" type="number" step="0.01" readonly />
    """
  end

  @spec render_notes_field(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_notes_field(assigns) do
    ~H"""
    <.form_field field={@form[:notes]} label="Payment Notes" type="textarea" />
    """
  end

  @spec render_form_actions(loading :: boolean(), on_cancel :: String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div>
      <.button type="submit" variant="primary" loading={@loading}>Process Payment</.button>
      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates payment form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_payment_data(map()) :: {:ok, payment_form_data()} | {:error, map()}
  def validate_payment_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_payment_method_fields(params) do
      {:ok, transform_payment_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:payment_method]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_payment_method_fields(params) when is_map(params) do
    payment_method = Map.get(params, :payment_method)

    case payment_method do
      "credit_card" -> validate_credit_card_fields(params)
      "bank_transfer" -> validate_bank_transfer_fields(params)
      "cash" -> validate_cash_fields(params)
      _ -> {:error, %{payment_method: "invalid payment method"}}
    end
  end

  defp validate_credit_card_fields(params) when is_map(params) do
    required_fields = [:card_number, :card_holder_name, :expiry_month, :expiry_year, :cvv]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_bank_transfer_fields(params) when is_map(params) do
    required_fields = [:bank_account, :routing_number]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_cash_fields(params) when is_map(params) do
    case params[:amount_received] do
      nil -> {:error, %{amount_received: "amount received is required for cash payments"}}
      amount when is_number(amount) and amount >= 0 -> :ok
      _ -> {:error, %{amount_received: "must be a positive number"}}
    end
  end

  @doc """
  Transforms raw payment data into structured format.
  """
  @spec transform_payment_data(map()) :: payment_form_data()
  def transform_payment_data(params) do
    %{
      payment_method: Map.get(params, :payment_method, "") |> String.trim(),
      card_number: Map.get(params, :card_number, "") |> String.trim(),
      card_holder_name: Map.get(params, :card_holder_name, "") |> String.trim(),
      expiry_month: Map.get(params, :expiry_month, "") |> String.trim(),
      expiry_year: Map.get(params, :expiry_year, "") |> String.trim(),
      cvv: Map.get(params, :cvv, "") |> String.trim(),
      bank_account: Map.get(params, :bank_account, "") |> String.trim(),
      routing_number: Map.get(params, :routing_number, "") |> String.trim(),
      amount_received: Map.get(params, :amount_received, 0.0) |> parse_float(),
      change_given: Map.get(params, :change_given, 0.0) |> parse_float(),
      notes: Map.get(params, :notes, "") |> String.trim()
    }
  end

  defp parse_float(nil), do: nil
  defp parse_float(value) when is_binary(value), do: Float.parse(value) |> elem(0)
  defp parse_float(value) when is_float(value), do: value
  defp parse_float(value) when is_integer(value), do: value / 1.0
  defp parse_float(_), do: nil

  @doc """
  Formats currency values for display.
  """
  @spec format_currency(number() | nil) :: String.t()
  def format_currency(nil), do: "0.00"
  def format_currency(amount) when is_number(amount), do: :erlang.float_to_binary(amount, decimals: 2)

  # UI Helper functions

  @spec payment_method_options(list()) :: list({String.t(), String.t()})
  defp payment_method_options(payment_methods) when is_list(payment_methods) do
    Enum.map(payment_methods, fn method ->
      {Atom.to_string(method) |> String.capitalize(), Atom.to_string(method)}
    end)
  end

  @spec expiry_month_options() :: list({String.t(), String.t()})
  defp expiry_month_options do
    Application.compile_env(:riva_ash, :expiry_month_options, [
      {"01", "01"},
      {"02", "02"},
      {"03", "03"},
      {"04", "04"},
      {"05", "05"},
      {"06", "06"},
      {"07", "07"},
      {"08", "08"},
      {"09", "09"},
      {"10", "10"},
      {"11", "11"},
      {"12", "12"}
    ])
  end

  @spec expiry_year_options() :: list({String.t(), String.t()})
  defp expiry_year_options do
    current_year = DateTime.utc_now().year
    years = current_year..(current_year + 10)

    Enum.map(years, fn year ->
      {to_string(year), to_string(year)}
    end)
  end

  @spec show_credit_card_fields?(String.t() | nil) :: boolean()
  defp show_credit_card_fields?("credit_card"), do: true
  defp show_credit_card_fields?(_), do: false

  @spec show_bank_transfer_fields?(String.t() | nil) :: boolean()
  defp show_bank_transfer_fields?("bank_transfer"), do: true
  defp show_bank_transfer_fields?(_), do: false

  @spec show_cash_fields?(String.t() | nil) :: boolean()
  defp show_cash_fields?("cash"), do: true
  defp show_cash_fields?(_), do: false
end
