alias RivaAshWeb.Components.Forms, as: Forms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.Interactive, as: Interactive
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Forms.ReservationBookingForm do
  @moduledoc """
  Reservation booking form component.

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
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Interactive.TimeSlotPicker

  @type assigns :: %{
          optional(:form) => map(),
          optional(:step) => integer(),
          optional(:total_steps) => integer(),
          optional(:clients) => list(),
          optional(:items) => list(),
          optional(:available_slots) => list(),
          optional(:selected_slots) => list(),
          optional(:pricing_info) => map(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_next_step) => String.t(),
          optional(:on_prev_step) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:on_slot_select) => String.t(),
          optional(:on_slot_deselect) => String.t(),
          optional(:loading) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => any()
        }

  @type booking_form_data :: %{
          client_id: String.t() | integer(),
          client_notes: String.t(),
          item_id: String.t() | integer(),
          reservation_date: Date.t(),
          payment_method: String.t(),
          selected_slots: list()
        }

  @type pricing_info :: %{
          subtotal: float(),
          tax: float(),
          total: float()
        }

  @doc """
  Renders a reservation booking form.

  ## Examples
      <.reservation_booking_form
        form={@form}
        step={@step}
        total_steps={@total_steps}
        clients={@clients}
        items={@items}
        available_slots={@available_slots}
        selected_slots={@selected_slots}
        pricing_info={@pricing_info}
        on_submit="confirm_reservation"
        on_change="update_form"
        on_next_step="next_step"
        on_prev_step="prev_step"
        on_cancel="cancel_booking"
        on_slot_select="select_slot"
        on_slot_deselect="deselect_slot"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:step, :integer, default: 1)
  attr(:total_steps, :integer, default: 4)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:available_slots, :list, default: [])
  attr(:selected_slots, :list, default: [])
  attr(:pricing_info, :map, default: %{})
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_next_step, :string, required: true)
  attr(:on_prev_step, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:on_slot_select, :string, required: true)
  attr(:on_slot_deselect, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec reservation_booking_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def reservation_booking_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_reservation_booking_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:step, 1)
    |> Map.put_new(:total_steps, 4)
    |> Map.put_new(:clients, [])
    |> Map.put_new(:items, [])
    |> Map.put_new(:available_slots, [])
    |> Map.put_new(:selected_slots, [])
    |> Map.put_new(:pricing_info, %{})
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_reservation_booking_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_reservation_booking_form(assigns) do
    ~H"""
    <div class={@class} {@rest}>
      <.render_progress_bar step={@step} total_steps={@total_steps} />
      <form phx-submit={@on_submit} phx-change={@on_change}>
        <.render_current_step
          step={@step}
          form={@form}
          clients={@clients}
          items={@items}
          available_slots={@available_slots}
          selected_slots={@selected_slots}
          pricing_info={@pricing_info}
        />
        <.render_step_navigation
          step={@step}
          total_steps={@total_steps}
          loading={@loading}
          on_prev_step={@on_prev_step}
          on_next_step={@on_next_step}
          on_submit={@on_submit}
          on_cancel={@on_cancel}
        />
      </form>
    </div>
    """
  end

  @spec render_progress_bar(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_progress_bar(assigns) do
    ~H"""
    <div>
      <h2>Book Reservation - Step <%= @step %> of <%= @total_steps %></h2>
      <div class="progress-bar">
        <div style={"width: #{@step / @total_steps * 100}%"}></div>
      </div>
    </div>
    """
  end

  @spec render_current_step(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_current_step(assigns) do
    ~H"""
    <div :if={@step == 1}>
      <.render_client_selection form={@form} clients={@clients} />
    </div>
    <div :if={@step == 2}>
      <.render_item_selection form={@form} items={@items} />
    </div>
    <div :if={@step == 3}>
      <.render_datetime_selection form={@form} available_slots={@available_slots} selected_slots={@selected_slots} />
    </div>
    <div :if={@step == 4}>
      <.render_confirmation form={@form} clients={@clients} items={@items} selected_slots={@selected_slots} pricing_info={@pricing_info} />
    </div>
    """
  end

  @spec render_client_selection(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_client_selection(assigns) do
    ~H"""
    <h3>Select Client</h3>
    <.select_field field={@form[:client_id]} label="Client" options={@clients} required />
    <.form_field field={@form[:client_notes]} label="Special Requests" type="textarea" />
    """
  end

  @spec render_item_selection(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_item_selection(assigns) do
    ~H"""
    <h3>Select Item</h3>
    <.select_field field={@form[:item_id]} label="Item" options={@items} required />
    <div :if={@form[:item_id].value}>
      <.render_item_details item={find_item(@items, @form[:item_id].value)} />
    </div>
    """
  end

  @spec render_datetime_selection(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_datetime_selection(assigns) do
    ~H"""
    <h3>Select Date & Time</h3>
    <.date_picker field={@form[:reservation_date]} required />
    <div :if={@form[:reservation_date].value}>
      <.time_slot_picker
        available_slots={@available_slots}
        selected_slots={@selected_slots}
        on_slot_select={@on_slot_select}
        on_slot_deselect={@on_slot_deselect}
      />
    </div>
    """
  end

  @spec render_confirmation(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_confirmation(assigns) do
    ~H"""
    <h3>Confirm Reservation</h3>
    <.render_reservation_summary
      form={@form}
      clients={@clients}
      items={@items}
      selected_slots={@selected_slots}
    />
    <.render_pricing_summary pricing_info={@pricing_info} />
    <.render_payment_selection form={@form} />
    """
  end

  @spec render_item_details(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_item_details(assigns) do
    ~H"""
    <div>
      <h4>Item Details</h4>
      <p>Name: <%= @item.name %></p>
      <p>Description: <%= @item.description %></p>
      <p>Price: $<%= format_currency(@item.price) %></p>
    </div>
    """
  end

  @spec render_reservation_summary(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_reservation_summary(assigns) do
    ~H"""
    <div>
      <h4>Reservation Summary</h4>
      <p>Client: <%= get_client_name(@clients, @form[:client_id].value) %></p>
      <p>Item: <%= get_item_name(@items, @form[:item_id].value) %></p>
      <p>Date: <%= format_date(@form[:reservation_date].value) %></p>
      <p>Time Slots: <%= length(@selected_slots) %> selected</p>
    </div>
    """
  end

  @spec render_pricing_summary(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_pricing_summary(assigns) do
    ~H"""
    <div :if={@pricing_info != %{}}>
      <h4>Pricing</h4>
      <p>Subtotal: $<%= format_currency(@pricing_info.subtotal) %></p>
      <p>Tax: $<%= format_currency(@pricing_info.tax) %></p>
      <p>Total: $<%= format_currency(@pricing_info.total) %></p>
    </div>
    """
  end

  @spec render_payment_selection(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_payment_selection(assigns) do
    ~H"""
    <.select_field field={@form[:payment_method]} label="Payment Method" options={payment_method_options()} required />
    """
  end

  @spec render_step_navigation(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_step_navigation(assigns) do
    ~H"""
    <div>
      <.button
        :if={@step > 1}
        type="button"
        variant="outline"
        phx-click={@on_prev_step}
      >
        Previous
      </.button>

      <.button
        :if={@step < @total_steps}
        type="button"
        phx-click={@on_next_step}
      >
        Next
      </.button>

      <.button
        :if={@step == @total_steps}
        type="submit"
        loading={@loading}
      >
        Confirm Reservation
      </.button>

      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates reservation booking form data for current step.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_booking_step_data(map(), integer()) :: {:ok, booking_form_data()} | {:error, map()}
  def validate_booking_step_data(params, step) when is_map(params) and is_integer(step) do
    case step do
      1 -> validate_client_selection(params)
      2 -> validate_item_selection(params)
      3 -> validate_datetime_selection(params)
      4 -> validate_confirmation_data(params)
      _ -> {:error, %{step: "invalid step"}}
    end
  end

  # Guard clauses for early validation
  defp validate_client_selection(params) when is_map(params) do
    with :ok <- validate_required_fields(params, [:client_id]),
         :ok <- validate_client_exists(params[:client_id]) do
      {:ok, transform_client_selection_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_item_selection(params) when is_map(params) do
    with :ok <- validate_required_fields(params, [:item_id]),
         :ok <- validate_item_exists(params[:item_id]) do
      {:ok, transform_item_selection_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_datetime_selection(params) when is_map(params) do
    with :ok <- validate_required_fields(params, [:reservation_date]),
         :ok <- validate_date(params[:reservation_date]) do
      {:ok, transform_datetime_selection_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_confirmation_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params, [:payment_method]),
         :ok <- validate_payment_method(params[:payment_method]) do
      {:ok, transform_confirmation_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_required_fields(params, fields) when is_map(params) and is_list(fields) do
    missing_fields = fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_client_exists(nil), do: {:error, %{client_id: "client is required"}}

  defp validate_client_exists(_client_id), do: :ok

  defp validate_item_exists(nil), do: {:error, %{item_id: "item is required"}}

  defp validate_item_exists(_item_id), do: :ok

  defp validate_date(nil), do: {:error, %{reservation_date: "date is required"}}
  defp validate_date(%Date{} = _date), do: :ok
  defp validate_date(_), do: {:error, %{reservation_date: "invalid date format"}}

  defp validate_payment_method(nil), do: {:error, %{payment_method: "payment method is required"}}

  defp validate_payment_method(method) when is_binary(method) do
    valid_methods = ["cash", "credit_card", "bank_transfer"]

    if method in valid_methods do
      :ok
    else
      {:error, %{payment_method: "must be one of: #{Enum.join(valid_methods, ", ")}"}}
    end
  end

  defp validate_payment_method(_), do: {:error, %{payment_method: "must be a string"}}

  @doc """
  Transforms raw booking data into structured format based on step.
  """
  @spec transform_client_selection_data(map()) :: booking_form_data()
  def transform_client_selection_data(params) do
    %{
      client_id: parse_id(params[:client_id]),
      client_notes: Map.get(params, :client_notes, "") |> String.trim(),
      item_id: nil,
      reservation_date: nil,
      payment_method: nil,
      selected_slots: []
    }
  end

  @spec transform_item_selection_data(map()) :: booking_form_data()
  def transform_item_selection_data(params) do
    %{
      client_id: nil,
      client_notes: "",
      item_id: parse_id(params[:item_id]),
      reservation_date: nil,
      payment_method: nil,
      selected_slots: []
    }
  end

  @spec transform_datetime_selection_data(map()) :: booking_form_data()
  def transform_datetime_selection_data(params) do
    %{
      client_id: nil,
      client_notes: "",
      item_id: nil,
      reservation_date: parse_date(params[:reservation_date]),
      payment_method: nil,
      selected_slots: Map.get(params, :selected_slots, [])
    }
  end

  @spec transform_confirmation_data(map()) :: booking_form_data()
  def transform_confirmation_data(params) do
    %{
      client_id: nil,
      client_notes: "",
      item_id: nil,
      reservation_date: nil,
      payment_method: Map.get(params, :payment_method, "") |> String.trim(),
      selected_slots: []
    }
  end

  # Data transformation helpers
  defp parse_id(nil), do: nil
  defp parse_id(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_id(value) when is_integer(value), do: value
  defp parse_id(_), do: nil

  defp parse_date(nil), do: nil
  defp parse_date(%Date{} = date), do: date

  defp parse_date(date_str) when is_binary(date_str) do
    case Date.from_iso8601(date_str) do
      {:ok, date} -> date
      _ -> nil
    end
  end

  defp parse_date(_), do: nil

  # UI Helper functions
  defp get_client_name(clients, client_id) do
    case Enum.find(clients, fn {_name, id} -> id == client_id end) do
      {name, _id} -> name
      _ -> "Unknown"
    end
  end

  defp get_item_name(items, item_id) do
    case Enum.find(items, fn {_name, id} -> id == item_id end) do
      {name, _id} -> name
      _ -> "Unknown"
    end
  end

  defp find_item(items, item_id) when is_list(items) do
    case Enum.find(items, fn {_name, id} -> id == item_id end) do
      {_name, ^item_id} = item -> item
      _ -> nil
    end
  end

  defp format_currency(nil), do: "0.00"
  defp format_currency(amount) when is_number(amount), do: :erlang.float_to_binary(amount, decimals: 2)

  defp format_date(nil), do: ""
  defp format_date(%Date{} = date), do: Date.to_string(date)
  defp format_date(_), do: ""

  @payment_method_options Application.compile_env(:riva_ash, :payment_method_options, [
                            {"Cash", "cash"},
                            {"Credit Card", "credit_card"},
                            {"Bank Transfer", "bank_transfer"}
                          ])

  defp payment_method_options, do: @payment_method_options
end
