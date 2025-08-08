alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Organisms.ReservationForm do
  @moduledoc """
  Complex reservation creation form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.DatePicker
  import RivaAshWeb.Components.Atoms.TimePicker

  @doc """
  Renders a reservation form with all necessary fields.
  """
  attr(:form, :map, required: true)
  attr(:clients, :list, default: [])
  attr(:items, :list, default: [])
  attr(:employees, :list, default: [])
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:step, :integer, default: 1)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @spec reservation_form(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def reservation_form(assigns) do
    # Render reservation form using functional composition
    assigns
    |> Map.put_new(:form_class, build_form_class(assigns.class, assigns.variant))
    |> Map.put_new(:step1_class, build_step1_class(assigns.step, assigns.form))
    |> Map.put_new(:step2_class, build_step2_class(assigns.step, assigns.form))
    |> Map.put_new(:client_field_class, build_client_field_class(assigns.clients))
    |> Map.put_new(:item_field_class, build_item_field_class(assigns.items))
    |> Map.put_new(:date_field_class, build_date_field_class(assigns.form))
    |> Map.put_new(:time_fields_class, build_time_fields_class(assigns.form))
    |> Map.put_new(:actions_class, build_actions_class(assigns.loading))
    |> render_reservation_form_component()
  end

  # Private helper for reservation form rendering
  @spec render_reservation_form_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_reservation_form_component(assigns) do
    ~H"""
    <form class={@form_class} phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <div class={@step1_class}>
        <!-- Step 1: Basic info -->
        <.select_field
          field={@form[:client_id]}
          label="Client"
          options={Enum.map(@clients, &{&1.name, &1.id})}
          class={@client_field_class}
        />
        <.select_field
          field={@form[:item_id]}
          label="Item"
          options={Enum.map(@items, &{&1.name, &1.id})}
          class={@item_field_class}
        />
      </div>

      <div class={@step2_class}>
        <!-- Step 2: Date/Time -->
        <.date_picker
          field={@form[:reserved_date]}
          placeholder="Reservation Date"
          class={@date_field_class}
        />
        <.time_picker
          field={@form[:start_time]}
          placeholder="Start Time"
          class={@time_fields_class}
        />
        <.time_picker
          field={@form[:end_time]}
          placeholder="End Time"
          class={@time_fields_class}
        />
      </div>

      <div class={@actions_class}>
        <.button type="submit" variant="primary" loading={@loading}>
          Create Reservation
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end

  # Helper function to build form classes
  @spec build_form_class(String.t(), String.t()) :: String.t()
  defp build_form_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-3"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-4"
        _unmatchedunmatched -> "space-y-4"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build step 1 container classes
  @spec build_step1_class(integer(), map()) :: String.t()
  defp build_step1_class(step, form) do
    if step == 1 and (form[:client_id] or form[:item_id]), do: "space-y-4", else: "hidden"
  end

  # Helper function to build step 2 container classes
  @spec build_step2_class(integer(), map()) :: String.t()
  defp build_step2_class(step, form) do
    if step == 2 and (form[:reserved_date] or form[:start_time]), do: "space-y-4", else: "hidden"
  end

  # Helper function to build client field classes
  @spec build_client_field_class(list()) :: String.t()
  defp build_client_field_class(clients) do
    if clients != [], do: "mb-4", else: "hidden"
  end

  # Helper function to build item field classes
  @spec build_item_field_class(list()) :: String.t()
  defp build_item_field_class(items) do
    if items != [], do: "mb-4", else: "hidden"
  end

  # Helper function to build date field classes
  @spec build_date_field_class(map()) :: String.t()
  defp build_date_field_class(form) do
    if form[:reserved_date], do: "mb-4", else: "hidden"
  end

  # Helper function to build time fields container classes
  @spec build_time_fields_class(map()) :: String.t()
  defp build_time_fields_class(form) do
    if form[:start_time] or form[:end_time], do: "mb-4", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class(boolean()) :: String.t()
  defp build_actions_class(loading) do
    "flex gap-3"
  end
end
