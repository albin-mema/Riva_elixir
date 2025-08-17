defmodule RivaAsh.Stories.DateRangePickerStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.DateRangePicker
  alias RivaAsh.Components.UI.Atoms, as: UI
  alias RivaAsh.Components.Templates.FormViewTemplate

  # Helper for date range state management
  defp date_range_state(assigns) do
    assigns =
      assigns
      |> assign_new(:start_date, fn -> Date.utc_today() end)
      |> assign_new(:end_date, fn -> Date.add(Date.utc_today(), 7) end)
      |> assign_new(:error, fn -> nil end)

    ~H"""
    <div phx-hook="DateRangeState">
      <.date_range_picker
        id="story-date-range"
        start_date={@start_date}
        end_date={@end_date}
        on_change={fn dates -> send(self(), {:update_dates, dates}) end}
        on_apply={fn -> send(self(), :apply) end}
        error={@error}
        label="Select reservation dates"
      />
    </div>
    """
  end

  # Basic date range selection flow
  def basic_date_range_selection(assigns) do
    ~H"""
    <FormViewTemplate title="Basic Date Range Selection">
      <:content>
        <.date_range_state />
      </:content>
    </FormViewTemplate>
    """
  end

  # Preset range interactions
  def preset_interactions(assigns) do
    ~H"""
    <FormViewTemplate title="Preset Range Interactions">
      <:content>
        <.date_range_state
          start_date={Date.add(Date.utc_today(), -3)}
          end_date={Date.add(Date.utc_today(), 3)}
        />
      </:content>
    </FormViewTemplate>
    """
  end

  # Keyboard navigation through both pickers
  def keyboard_navigation(assigns) do
    ~H"""
    <FormViewTemplate title="Keyboard Navigation">
      <:content>
        <div class="p-4">
          <p class="mb-4 text-muted-foreground">
            Focus the first date picker, then use:
          </p>
          <ul class="mb-4 pl-6 list-disc">
            <li>Tab/Shift+Tab to move between pickers</li>
            <li>Arrow keys to navigate calendar</li>
            <li>Enter to select date</li>
            <li>ArrowLeft/ArrowRight to navigate presets</li>
          </ul>
          <.date_range_state />
        </div>
      </:content>
    </FormViewTemplate>
    """
  end

  # Error states for invalid ranges
  def error_states(assigns) do
    ~H"""
    <FormViewTemplate title="Error States">
      <:content>
        <div class="space-y-6">
          <div>
            <h3 class="mb-2 font-medium text-lg">End date before start date</h3>
            <.date_range_state
              start_date={Date.add(Date.utc_today(), 5)}
              end_date={Date.utc_today()}
              error="End date must be after start date"
            />
          </div>

          <div>
            <h3 class="mb-2 font-medium text-lg">Range exceeds maximum (30 days)</h3>
            <.date_range_state
              start_date={Date.utc_today()}
              end_date={Date.add(Date.utc_today(), 31)}
              error="Date range cannot exceed 30 days"
            />
          </div>
        </div>
      </:content>
    </FormViewTemplate>
    """
  end

  # Integration with FormViewTemplate
  def form_integration(assigns) do
    ~H"""
    <FormViewTemplate
      title="Reservation Dates"
      description="Select your preferred reservation period"
      actions={
        ~H[
          <.ui.button>Cancel</.ui.button>
          <.ui.button variant="primary">Continue</.ui.button>
        ]
      }
    >
      <:content>
        <div class="max-w-2xl">
          <.ui.form_field label="Reservation Period" required>
            <.date_range_state />
          </.ui.form_field>

          <div class="bg-muted mt-6 p-4 rounded-lg">
            <h3 class="mb-2 font-medium">Selected Range</h3>
            <p>Start: {Date.to_iso8601(assigns.start_date)}</p>
            <p>End: {Date.to_iso8601(assigns.end_date)}</p>
          </div>
        </div>
      </:content>
    </FormViewTemplate>
    """
  end

  # Mobile viewport behavior (stacked pickers)
  def mobile_behavior(assigns) do
    ~H"""
    <div class="md:hidden">
      <FormViewTemplate title="Mobile Date Range Picker">
        <:content>
          <.date_range_state />

          <div class="bg-muted mt-6 p-4 rounded-lg">
            <h3 class="mb-2 font-medium">Mobile Behavior Notes</h3>
            <ul class="pl-6 list-disc">
              <li>Date pickers stack vertically on mobile</li>
              <li>Presets wrap to multiple lines</li>
              <li>Touch targets are enlarged for mobile</li>
              <li>Keyboard navigation adapted for touch</li>
            </ul>
          </div>
        </:content>
      </FormViewTemplate>
    </div>
    """
  end
end
