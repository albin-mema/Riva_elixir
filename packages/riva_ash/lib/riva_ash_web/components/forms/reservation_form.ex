defmodule RivaAshWeb.Components.Forms.ReservationForm do
  @moduledoc """
  Reservation form component using atomic design system.
  
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
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Atoms.TimePicker

  @type assigns :: %{
    optional(:form) => map(),
    optional(:editing) => boolean(),
    optional(:loading) => boolean(),
    optional(:clients) => list(),
    optional(:items) => list(),
    optional(:employees) => list(),
    optional(:on_submit) => String.t(),
    optional(:on_change) => String.t(),
    optional(:on_cancel) => String.t(),
    optional(:class) => String.t(),
    optional(:rest) => any()
  }

  @type reservation_form_data :: %{
    client_id: String.t() | integer(),
    item_id: String.t() | integer(),
    reserved_from: DateTime.t(),
    reserved_until: DateTime.t(),
    employee_id: String.t() | integer() | nil,
    notes: String.t()
  }

  @doc """
  Renders a reservation form for creating or editing reservations.
  
  ## Examples
      <.reservation_form
        form={@form}
        editing={@editing}
        loading={@loading}
        clients={@clients}
        items={@items}
        employees={@employees}
        on_submit="save_reservation"
        on_change="validate_reservation"
        on_cancel="cancel_reservation"
      />
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:employees, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec reservation_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def reservation_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_reservation_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:clients, [])
    |> Map.put_new(:items, [])
    |> Map.put_new(:employees, [])
    |> Map.put_new(:class, "")
  end

  @spec render_reservation_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_reservation_form(assigns) do
    ~H"""
    <.card variant="elevated" class={@class} {@rest}>
      <:body>
        <form phx-submit={@on_submit} phx-change={@on_change} class="space-y-6">
          <.render_client_selection clients={@clients} />
          <.render_item_selection items={@items} />
          <.render_datetime_selection />
          <.render_employee_selection employees={@employees} />
          <.render_notes_field />
          <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
        </form>
      </:body>
    </.card>
    """
  end

  @spec render_client_selection(clients :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_client_selection(assigns) do
    ~H"""
    <.select_field
      field={@form[:client_id]}
      label="Client"
      options={@clients}
      required
      helper_text="Select the client for this reservation"
    />
    """
  end

  @spec render_item_selection(items :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_item_selection(assigns) do
    ~H"""
    <.select_field
      field={@form[:item_id]}
      label="Item"
      options={@items}
      required
      helper_text="Select the item to reserve"
    />
    """
  end

  @spec render_datetime_selection() :: Phoenix.LiveView.Rendered.t()
  defp render_datetime_selection(assigns \\ %{}) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
      <.date_picker field={@form[:reserved_from]} required />
      <.time_picker field={@form[:reserved_from]} required />
    </div>
    <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
      <.date_picker field={@form[:reserved_until]} required />
      <.time_picker field={@form[:reserved_until]} required />
    </div>
    """
  end

  @spec render_employee_selection(employees :: list()) :: Phoenix.LiveView.Rendered.t()
  defp render_employee_selection(assigns) do
    ~H"""
    <.select_field
      field={@form[:employee_id]}
      label="Employee"
      options={@employees}
      helper_text="Select the employee handling this reservation (optional)"
    />
    """
  end

  @spec render_notes_field() :: Phoenix.LiveView.Rendered.t()
  defp render_notes_field(assigns \\ %{}) do
    ~H"""
    <.form_field
      field={@form[:notes]}
      label="Notes"
      type="textarea"
      helper_text="Add any special notes for this reservation"
    />
    """
  end

  @spec render_form_actions(editing :: boolean(), loading :: boolean(), on_cancel :: String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div class="flex justify-end space-x-3 pt-4 border-t">
      <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
        Cancel
      </.button>
      <.button type="submit" loading={@loading}>
        <%= if @editing, do: "Update Reservation", else: "Create Reservation" %>
      </.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates reservation form data.
  
  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_reservation_data(map()) :: {:ok, reservation_form_data()} | {:error, map()}
  def validate_reservation_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_datetime_range(params),
         :ok <- validate_client_exists(params[:client_id], @clients),
         :ok <- validate_item_exists(params[:item_id], @items) do
      {:ok, transform_reservation_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:client_id, :item_id, :reserved_from, :reserved_until]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_datetime_range(params) when is_map(params) do
    with {:ok, from} <- get_datetime(params[:reserved_from]),
         {:ok, until} <- get_datetime(params[:reserved_until]) do
      if DateTime.compare(from, until) == :lt do
        :ok
      else
        {:error, %{datetime_range: "Reserved from must be before reserved until"}}
      end
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_client_exists(nil, _), do: :ok
  defp validate_client_exists(client_id, clients) when is_list(clients) do
    case Enum.find(clients, fn {_, id} -> id == client_id end) do
      {_name, ^client_id} -> :ok
      _ -> {:error, %{client_id: "Client not found"}}
    end
  end

  defp validate_item_exists(nil, _), do: :ok
  defp validate_item_exists(item_id, items) when is_list(items) do
    case Enum.find(items, fn {_, id} -> id == item_id end) do
      {_name, ^item_id} -> :ok
      _ -> {:error, %{item_id: "Item not found"}}
    end
  end

  defp get_datetime(nil), do: {:error, %{datetime: "DateTime is required"}}
  defp get_datetime(%DateTime{} = dt), do: {:ok, dt}
  defp get_datetime(%Date{} = d), do: {:ok, DateTime.new(d, ~T[00:00:00])}
  defp get_datetime(_), do: {:error, %{datetime: "Invalid datetime format"}}

  @doc """
  Transforms raw reservation data into structured format.
  """
  @spec transform_reservation_data(map()) :: reservation_form_data()
  def transform_reservation_data(params) do
    %{
      client_id: Map.get(params, :client_id),
      item_id: Map.get(params, :item_id),
      reserved_from: parse_datetime(params[:reserved_from]),
      reserved_until: parse_datetime(params[:reserved_until]),
      employee_id: Map.get(params, :employee_id),
      notes: Map.get(params, :notes, "") |> String.trim()
    }
  end

  defp parse_datetime(%DateTime{} = dt), do: dt
  defp parse_datetime(%Date{} = d), do: DateTime.new(d, ~T[00:00:00]) |> elem(1)
  defp parse_datetime(_), do: nil
end
