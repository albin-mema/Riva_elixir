alias RivaAshWeb.Components.Forms, as: Forms
alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias Phoenix.LiveView.Rendered, as: Rendered
alias RivaAshWeb.Components.UI.Select, as: Select

defmodule RivaAshWeb.Components.Forms.RecurringReservationInstanceForm do
  @moduledoc """
  Recurring reservation instance form component.

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
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.FormField

  @type assigns :: %{
          optional(:form) => map(),
          optional(:editing) => boolean(),
          optional(:loading) => boolean(),
          optional(:recurring_reservations) => list(),
          optional(:reservations) => list(),
          optional(:on_submit) => String.t(),
          optional(:on_change) => String.t(),
          optional(:on_cancel) => String.t(),
          optional(:class) => String.t()
        }

  @type recurring_instance_form_data :: %{
          scheduled_date: Date.t(),
          sequence_number: integer(),
          status: String.t(),
          recurring_reservation_id: String.t() | integer(),
          notes: String.t(),
          skip_reason: String.t(),
          error_message: String.t(),
          created_at: DateTime.t(),
          failed_at: DateTime.t(),
          reservation_id: String.t() | integer()
        }

  @doc """
  Renders a recurring reservation instance form for creating or editing instances.

  ## Examples

      <.recurring_reservation_instance_form
        form={@form}
        editing={@editing_instance}
        loading={@loading}
        recurring_reservations={@recurring_reservations}
        on_submit="save_instance"
        on_change="validate_instance"
        on_cancel="cancel_form"
      />
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:loading, :boolean, default: false)
  attr(:recurring_reservations, :list, default: [])
  attr(:reservations, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:class, :string, default: "")

  @spec recurring_reservation_instance_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def recurring_reservation_instance_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_recurring_reservation_instance_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:editing, false)
    |> Map.put_new(:loading, false)
    |> Map.put_new(:recurring_reservations, [])
    |> Map.put_new(:reservations, [])
    |> Map.put_new(:class, "")
  end

  @spec render_recurring_reservation_instance_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_recurring_reservation_instance_form(assigns) do
    ~H"""
    <.card variant="elevated" class={@class}>
      <:header>
        <.render_header editing={@editing} />
      </:header>

      <:body>
        <.form
          for={@form}
          phx-submit={@on_submit}
          phx-change={@on_change}
          class="space-y-6"
        >
          <.render_basic_fields form={@form} />
          <.render_status_and_reservation_fields
            form={@form}
            recurring_reservations={@recurring_reservations}
          />
          <.render_notes_and_skip_fields form={@form} />
          <%= if @editing do %>
            <.render_editing_fields
              form={@form}
              reservations={@reservations}
            />
          <% end %>
          <.render_form_actions editing={@editing} loading={@loading} on_cancel={@on_cancel} />
        </.form>
      </:body>
    </.card>
    """
  end

  @spec render_header(editing :: boolean()) :: Phoenix.LiveView.Rendered.t()
  defp render_header(assigns) do
    ~H"""
    <.card_title>
      <%= if @editing, do: "Edit Instance", else: "Add New Instance" %>
    </.card_title>
    <.card_description>
      <%= if @editing do %>
        Update the recurring reservation instance information below.
      <% else %>
        Fill in the details to add a new recurring reservation instance.
      <% end %>
    </.card_description>
    """
  end

  @spec render_basic_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_basic_fields(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
      <.form_field
        field={@form[:scheduled_date]}
        label="Scheduled Date"
        type="date"
        required={true}
      />

      <.form_field
        field={@form[:sequence_number]}
        label="Sequence Number"
        type="number"
        required={true}
        helper_text="The sequence number in the recurring pattern (1, 2, 3, etc.)"
      />
    </div>
    """
  end

  @spec render_status_and_reservation_fields(assigns :: map()) ::
          Phoenix.LiveView.Rendered.t()
  defp render_status_and_reservation_fields(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
      <.form_field
        field={@form[:status]}
        label="Status"
        type="select"
        required={true}
      >
        <:input>
          <RivaAshWeb.Components.UI.Select.select
            options={status_options()}
            prompt="Select a status"
          />
        </:input>
      </.form_field>

      <%= if @recurring_reservations != [] do %>
        <.form_field
          field={@form[:recurring_reservation_id]}
          label="Recurring Reservation"
          type="select"
          required={true}
        >
          <:input>
            <RivaAshWeb.Components.UI.Select.select
              options={recurring_reservation_options(@recurring_reservations)}
              prompt="Select a recurring reservation"
            />
          </:input>
        </.form_field>
      <% end %>
    </div>
    """
  end

  @spec render_notes_and_skip_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_notes_and_skip_fields(assigns) do
    ~H"""
    <.form_field
      field={@form[:notes]}
      label="Notes"
      type="textarea"
      placeholder="Additional notes about this instance..."
      helper_text="Optional notes or comments"
    />

    <.form_field
      field={@form[:skip_reason]}
      label="Skip Reason"
      type="textarea"
      placeholder="Reason for skipping this instance..."
      helper_text="Only applicable if status is 'skipped'"
    />
    """
  end

  @spec render_editing_fields(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_editing_fields(assigns) do
    ~H"""
    <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
      <.form_field
        field={@form[:error_message]}
        label="Error Message"
        type="textarea"
        placeholder="Error message if reservation creation failed..."
        helper_text="Only applicable if status is 'failed'"
      />

      <.form_field
        field={@form[:created_at]}
        label="Created At"
        type="datetime-local"
        helper_text="When the actual reservation was created for this instance"
      />
    </div>

    <div class="gap-4 grid grid-cols-1 md:grid-cols-2">
      <.form_field
        field={@form[:failed_at]}
        label="Failed At"
        type="datetime-local"
        helper_text="When this instance failed to create a reservation"
      />

      <%= if @reservations != [] do %>
        <.form_field
          field={@form[:reservation_id]}
          label="Reservation"
          type="select"
        >
          <:input>
            <RivaAshWeb.Components.UI.Select.select
              options={reservation_options(@reservations)}
              prompt="Select a reservation"
            />
          </:input>
        </.form_field>
      <% end %>
    </div>
    """
  end

  @spec render_form_actions(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div class="flex justify-end space-x-3 pt-4 border-t">
      <.button
        type="button"
        variant="outline"
        phx-click={@on_cancel}
        disabled={@loading}
      >
        Cancel
      </.button>

      <.button
        type="submit"
        loading={@loading}
      >
        <%= if @editing, do: "Update Instance", else: "Add Instance" %>
      </.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates recurring reservation instance form data.

  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_recurring_instance_data(map()) :: {:ok, recurring_instance_form_data()} | {:error, map()}
  def validate_recurring_instance_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_date_fields(params),
         :ok <- validate_status_field(params[:status]) do
      {:ok, transform_recurring_instance_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:scheduled_date, :sequence_number, :status]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))

    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_date_fields(params) when is_map(params) do
    with {:ok, scheduled_date} <- validate_date(params[:scheduled_date]),
         :ok <- validate_sequence_number(params[:sequence_number]) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_date(nil), do: {:error, %{scheduled_date: "scheduled date is required"}}
  defp validate_date(%Date{} = _date), do: :ok
  defp validate_date(_), do: {:error, %{scheduled_date: "invalid date format"}}

  defp validate_sequence_number(nil), do: {:error, %{sequence_number: "sequence number is required"}}
  defp validate_sequence_number(number) when is_integer(number) and number > 0, do: :ok

  defp validate_sequence_number(number) when is_binary(number) do
    case Integer.parse(number) do
      {num, ""} when num > 0 -> :ok
      _ -> {:error, %{sequence_number: "must be a positive integer"}}
    end
  end

  defp validate_sequence_number(_), do: {:error, %{sequence_number: "must be a positive integer"}}

  defp validate_status_field(nil), do: {:error, %{status: "status is required"}}

  defp validate_status_field(status) when is_binary(status) do
    valid_statuses = ["pending", "confirmed", "failed", "skipped", "cancelled"]

    if status in valid_statuses do
      :ok
    else
      {:error, %{status: "must be one of: #{Enum.join(valid_statuses, ", ")}"}}
    end
  end

  defp validate_status_field(_), do: {:error, %{status: "must be a string"}}

  @doc """
  Transforms raw recurring instance data into structured format.
  """
  @spec transform_recurring_instance_data(map()) :: recurring_instance_form_data()
  def transform_recurring_instance_data(params) do
    %{
      scheduled_date: parse_date(params[:scheduled_date]),
      sequence_number: parse_integer(params[:sequence_number]),
      status: Map.get(params, :status, "pending") |> String.trim(),
      recurring_reservation_id: parse_id(params[:recurring_reservation_id]),
      notes: Map.get(params, :notes, "") |> String.trim(),
      skip_reason: Map.get(params, :skip_reason, "") |> String.trim(),
      error_message: Map.get(params, :error_message, "") |> String.trim(),
      created_at: parse_datetime(params[:created_at]),
      failed_at: parse_datetime(params[:failed_at]),
      reservation_id: parse_id(params[:reservation_id])
    }
  end

  defp parse_date(nil), do: nil
  defp parse_date(%Date{} = date), do: date

  defp parse_date(date_str) when is_binary(date_str) do
    case Date.from_iso8601(date_str) do
      {:ok, date} -> date
      _ -> nil
    end
  end

  defp parse_date(_), do: nil

  defp parse_integer(nil), do: nil
  defp parse_integer(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: nil

  defp parse_id(nil), do: nil
  defp parse_id(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_id(value) when is_integer(value), do: value
  defp parse_id(_), do: nil

  defp parse_datetime(nil), do: nil
  defp parse_datetime(%DateTime{} = dt), do: dt

  defp parse_datetime(datetime_str) when is_binary(datetime_str) do
    case DateTime.from_iso8601(datetime_str) do
      {:ok, dt, _offset} -> dt
      _ -> nil
    end
  end

  defp parse_datetime(_), do: nil

  # UI Helper functions

  @spec status_options() :: list({String.t(), String.t()})
  defp status_options do
    Application.get_env(:riva_ash, :recurring_instance_status_options, [
      {"Pending", "pending"},
      {"Confirmed", "confirmed"},
      {"Failed", "failed"},
      {"Skipped", "skipped"},
      {"Cancelled", "cancelled"}
    ])
  end

  @spec recurring_reservation_options(list()) :: list({String.t(), integer()})
  defp recurring_reservation_options(recurring_reservations) when is_list(recurring_reservations) do
    Enum.map(recurring_reservations, &{"Reservation ##{&1.id}", &1.id})
  end

  @spec reservation_options(list()) :: list({String.t(), integer()})
  defp reservation_options(reservations) when is_list(reservations) do
    [{"No reservation", ""} | Enum.map(reservations, &{"Reservation ##{&1.id}", &1.id})]
  end
end
