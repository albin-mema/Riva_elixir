defmodule RivaAshWeb.Components.Forms.ScheduleForm do
  @moduledoc """
  Item schedule configuration form component.
  
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
  import RivaAshWeb.Components.Atoms.TimePicker
  import RivaAshWeb.Components.Interactive.AvailabilityGrid

  @type assigns :: %{
    optional(:form) => map(),
    optional(:item) => map(),
    optional(:availability) => map(),
    optional(:on_submit) => String.t(),
    optional(:on_change) => String.t(),
    optional(:on_availability_change) => String.t(),
    optional(:on_cancel) => String.t(),
    optional(:loading) => boolean(),
    optional(:class) => String.t(),
    optional(:rest) => any()
  }

  @type schedule_form_data :: %{
    schedule_type: String.t(),
    season_start: Date.t(),
    season_end: Date.t(),
    advance_booking_days: integer(),
    max_booking_duration: integer(),
    min_booking_duration: integer(),
    slot_duration: integer(),
    earliest_start_time: Time.t(),
    latest_end_time: Time.t(),
    holiday_schedule: String.t()
  }

  @doc """
  Renders an item schedule configuration form.
  
  ## Examples
      <.schedule_form
        form={@form}
        item={@item}
        availability={@availability}
        on_submit="save_schedule"
        on_change="validate_schedule"
        on_availability_change="update_availability"
        on_cancel="cancel_schedule"
        loading={@loading}
      />
  """
  attr(:form, :map, required: true)
  attr(:item, :map, required: true)
  attr(:availability, :map, default: %{})
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_availability_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec schedule_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  def schedule_form(assigns) do
    assigns
    |> validate_assigns()
    |> render_schedule_form()
  end

  # Private functions for single level of abstraction

  @spec validate_assigns(assigns :: assigns()) :: assigns()
  defp validate_assigns(assigns) when is_map(assigns) do
    assigns
    |> Map.put_new(:availability, %{})
    |> Map.put_new(:loading, false)
    |> Map.put_new(:class, "")
  end

  @spec render_schedule_form(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_schedule_form(assigns) do
    ~H"""
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.render_header item={@item} />
      <.render_general_settings form={@form} />
      <.render_schedule_specific_fields form={@form} availability={@availability} />
      <.render_booking_rules form={@form} />
      <.render_time_slots form={@form} />
      <.render_exceptions form={@form} />
      <.render_form_actions loading={@loading} on_cancel={@on_cancel} />
    </form>
    """
  end

  @spec render_header(item :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_header(assigns) do
    ~H"""
    <div>
      <h3>Schedule for <%= @item.name %></h3>
    </div>
    """
  end

  @spec render_general_settings(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_general_settings(assigns) do
    ~H"""
    <div>
      <h4>General Settings</h4>
      <.select_field field={@form[:schedule_type]} label="Schedule Type" options={schedule_type_options()} required />
    </div>
    """
  end

  @spec render_schedule_specific_fields(form :: map(), availability :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_schedule_specific_fields(assigns) do
    ~H"""
    <div :if={show_custom_schedule?(@form[:schedule_type].value)}>
      <h4>Weekly Availability</h4>
      <.availability_grid
        availability={@availability}
        on_slot_toggle={@on_availability_change}
        on_bulk_action={@on_availability_change}
        start_hour={get_start_hour()}
        end_hour={get_end_hour()}
        editable={true}
      />
    </div>

    <div :if={show_seasonal_schedule?(@form[:schedule_type].value)}>
      <h4>Seasonal Settings</h4>
      <.date_picker field={@form[:season_start]} placeholder="Season Start Date" />
      <.date_picker field={@form[:season_end]} placeholder="Season End Date" />
    </div>
    """
  end

  @spec render_booking_rules(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_booking_rules(assigns) do
    ~H"""
    <div>
      <h4>Booking Rules</h4>
      <.form_field field={@form[:advance_booking_days]} label="Advance Booking (days)" type="number" />
      <.form_field field={@form[:max_booking_duration]} label="Max Booking Duration (hours)" type="number" />
      <.form_field field={@form[:min_booking_duration]} label="Min Booking Duration (hours)" type="number" />
    </div>
    """
  end

  @spec render_time_slots(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_time_slots(assigns) do
    ~H"""
    <div>
      <h4>Time Slots</h4>
      <.select_field field={@form[:slot_duration]} label="Slot Duration (minutes)" options={slot_duration_options()} />
      <.time_picker field={@form[:earliest_start_time]} placeholder="Earliest Start Time" />
      <.time_picker field={@form[:latest_end_time]} placeholder="Latest End Time" />
    </div>
    """
  end

  @spec render_exceptions(form :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_exceptions(assigns) do
    ~H"""
    <div>
      <h4>Exceptions</h4>
      <.select_field field={@form[:holiday_schedule]} label="Holiday Schedule" options={holiday_schedule_options()} />
    </div>
    """
  end

  @spec render_form_actions(loading :: boolean(), on_cancel :: String.t()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_actions(assigns) do
    ~H"""
    <div>
      <.button type="submit" loading={@loading}>Save Schedule</.button>
      <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
    </div>
    """
  end

  # Helper functions for data processing

  @doc """
  Validates schedule form data.
  
  ## Returns
    {:ok, validated_data} | {:error, changeset}
  """
  @spec validate_schedule_data(map()) :: {:ok, schedule_form_data()} | {:error, map()}
  def validate_schedule_data(params) when is_map(params) do
    with :ok <- validate_required_fields(params),
         :ok <- validate_schedule_type(params[:schedule_type]),
         :ok <- validate_booking_rules(params),
         :ok <- validate_time_slots(params) do
      {:ok, transform_schedule_data(params)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  # Guard clauses for early validation
  defp validate_required_fields(params) when is_map(params) do
    required_fields = [:schedule_type]
    missing_fields = required_fields |> Enum.filter(&is_nil(Map.get(params, &1)))
    
    case missing_fields do
      [] -> :ok
      _ -> {:error, %{missing_fields: missing_fields}}
    end
  end

  defp validate_schedule_type(nil), do: {:error, %{schedule_type: "schedule type is required"}}
  defp validate_schedule_type(type) when is_binary(type) do
    valid_types = ["always", "custom", "seasonal"]
    if type in valid_types do
      :ok
    else
      {:error, %{schedule_type: "must be one of: #{Enum.join(valid_types, ", ")}"}}
    end
  end
  defp validate_schedule_type(_), do: {:error, %{schedule_type: "must be a string"}}

  defp validate_booking_rules(params) when is_map(params) do
    with {:ok, advance_days} <- validate_advance_booking_days(params[:advance_booking_days]),
         {:ok, max_duration} <- validate_max_booking_duration(params[:max_booking_duration]),
         {:ok, min_duration} <- validate_min_booking_duration(params[:min_booking_duration]) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_advance_booking_days(nil), do: {:ok, nil}
  defp validate_advance_booking_days(days) when is_integer(days) and days >= 0, do: {:ok, days}
  defp validate_advance_booking_days(days) when is_binary(days) do
    case Integer.parse(days) do
      {num, ""} when num >= 0 -> {:ok, num}
      _ -> {:error, %{advance_booking_days: "must be a non-negative integer"}}
    end
  end
  defp validate_advance_booking_days(_), do: {:error, %{advance_booking_days: "must be a non-negative integer"}}

  defp validate_max_booking_duration(nil), do: {:ok, nil}
  defp validate_max_booking_duration(duration) when is_integer(duration) and duration > 0, do: {:ok, duration}
  defp validate_max_booking_duration(duration) when is_binary(duration) do
    case Integer.parse(duration) do
      {num, ""} when num > 0 -> {:ok, num}
      _ -> {:error, %{max_booking_duration: "must be a positive integer"}}
    end
  end
  defp validate_max_booking_duration(_), do: {:error, %{max_booking_duration: "must be a positive integer"}}

  defp validate_min_booking_duration(nil), do: {:ok, nil}
  defp validate_min_booking_duration(duration) when is_integer(duration) and duration >= 0, do: {:ok, duration}
  defp validate_min_booking_duration(duration) when is_binary(duration) do
    case Integer.parse(duration) do
      {num, ""} when num >= 0 -> {:ok, num}
      _ -> {:error, %{min_booking_duration: "must be a non-negative integer"}}
    end
  end
  defp validate_min_booking_duration(_), do: {:error, %{min_booking_duration: "must be a non-negative integer"}}

  defp validate_time_slots(params) when is_map(params) do
    with {:ok, slot_duration} <- validate_slot_duration(params[:slot_duration]),
         :ok <- validate_time_range(params) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp validate_slot_duration(nil), do: {:ok, nil}
  defp validate_slot_duration(duration) when is_integer(duration) and duration > 0, do: {:ok, duration}
  defp validate_slot_duration(duration) when is_binary(duration) do
    case Integer.parse(duration) do
      {num, ""} when num > 0 -> {:ok, num}
      _ -> {:error, %{slot_duration: "must be a positive integer"}}
    end
  end
  defp validate_slot_duration(_), do: {:error, %{slot_duration: "must be a positive integer"}}

  defp validate_time_range(params) when is_map(params) do
    with {:ok, earliest} <- parse_time(params[:earliest_start_time]),
         {:ok, latest} <- parse_time(params[:latest_end_time]) do
      if compare_times(earliest, latest) == :lt do
        :ok
      else
        {:error, %{time_range: "earliest start time must be before latest end time"}}
      end
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_time(nil), do: {:ok, nil}
  defp parse_time(%Time{} = time), do: {:ok, time}
  defp parse_time(time_str) when is_binary(time_str) do
    case Time.from_iso8601(time_str) do
      {:ok, time} -> {:ok, time}
      _ -> {:error, %{time: "invalid time format"}}
    end
  end
  defp parse_time(_), do: {:error, %{time: "invalid time format"}}

  defp compare_times(nil, _), do: :ok
  defp compare_times(_, nil), do: :ok
  defp compare_times(time1, time2), do: Time.compare(time1, time2)

  @doc """
  Transforms raw schedule data into structured format.
  """
  @spec transform_schedule_data(map()) :: schedule_form_data()
  def transform_schedule_data(params) do
    %{
      schedule_type: Map.get(params, :schedule_type, "always") |> String.trim(),
      season_start: parse_date(params[:season_start]),
      season_end: parse_date(params[:season_end]),
      advance_booking_days: parse_integer(params[:advance_booking_days]),
      max_booking_duration: parse_integer(params[:max_booking_duration]),
      min_booking_duration: parse_integer(params[:min_booking_duration]),
      slot_duration: parse_integer(params[:slot_duration]),
      earliest_start_time: parse_time(params[:earliest_start_time]),
      latest_end_time: parse_time(params[:latest_end_time]),
      holiday_schedule: Map.get(params, :holiday_schedule, "regular") |> String.trim()
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

  defp parse_time(nil), do: nil
  defp parse_time(%Time{} = time), do: time
  defp parse_time(time_str) when is_binary(time_str) do
    case Time.from_iso8601(time_str) do
      {:ok, time} -> time
      _ -> nil
    end
  end
  defp parse_time(_), do: nil

  defp parse_integer(nil), do: nil
  defp parse_integer(value) when is_binary(value), do: Integer.parse(value) |> elem(0)
  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: nil

  # UI Helper functions

  @spec schedule_type_options() :: list({String.t(), String.t()})
  defp schedule_type_options do
    Application.compile_env(:riva_ash, :schedule_type_options, [
      {"Always Available", "always"},
      {"Custom Schedule", "custom"},
      {"Seasonal", "seasonal"}
    ])
  end

  @spec slot_duration_options() :: list({String.t(), String.t()})
  defp slot_duration_options do
    Application.compile_env(:riva_ash, :slot_duration_options, [
      {"15 minutes", "15"},
      {"30 minutes", "30"},
      {"1 hour", "60"},
      {"2 hours", "120"},
      {"4 hours", "240"}
    ])
  end

  @spec holiday_schedule_options() :: list({String.t(), String.t()})
  defp holiday_schedule_options do
    Application.compile_env(:riva_ash, :holiday_schedule_options, [
      {"Follow regular schedule", "regular"},
      {"Closed on holidays", "closed"},
      {"Custom holiday hours", "custom"}
    ])
  end

  @spec get_start_hour() :: integer()
  defp get_start_hour, do: Application.compile_env(:riva_ash, :schedule_start_hour, 8)

  @spec get_end_hour() :: integer()
  defp get_end_hour, do: Application.compile_env(:riva_ash, :schedule_end_hour, 18)

  @spec show_custom_schedule?(String.t() | nil) :: boolean()
  defp show_custom_schedule?("custom"), do: true
  defp show_custom_schedule?(_), do: false

  @spec show_seasonal_schedule?(String.t() | nil) :: boolean()
  defp show_seasonal_schedule?("seasonal"), do: true
  defp show_seasonal_schedule?(_), do: false
end
