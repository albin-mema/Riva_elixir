defmodule RivaAsh.Components.UI.DateRangePicker do
  use Phoenix.Component
  alias RivaAsh.Components.UI.Atoms, as: UI

  attr :start_date, :date, required: true
  attr :end_date, :date, required: true
  attr :on_change, :function, required: true
  attr :on_apply, :function, required: true
  attr :class, :string, default: nil
  attr :presets, :list, default: [:today, :this_week, :this_month]
  attr :label, :string, default: "Select date range"
  attr :error, :string, default: nil
  attr :id, :string, default: "date-range-picker"

  def date_range_picker(assigns) do
    ~H"""
    <div
      id={@id}
      class={"date-range-picker #{assigns.class}"}
      role="group"
      aria-labelledby={"#{@id}-label"}
    >
      <span id={"#{@id}-label"} class="sr-only">{@label}</span>

      <!-- Date pickers with ARIA labels -->
      <div class="flex sm:flex-row flex-col gap-4 date-pickers" phx-hook="MotionFade">
        <.ui.date_picker
          id={"#{@id}-start"}
          label="Start date"
          date={@start_date}
          on_change={@on_change}
          class="w-full"
          aria-label="Start date selection"
          aria-controls={"#{@id}-end"}
        />
        <.ui.date_picker
          id={"#{@id}-end"}
          label="End date"
          date={@end_date}
          on_change={@on_change}
          class="w-full"
          aria-label="End date selection"
          aria-describedby={@error && "#{@id}-error"}
        />
      </div>

      <!-- Error state -->
      {#if @error}
        <div
          id={"#{@id}-error"}
          class="mt-2 text-destructive text-sm error"
          role="alert"
          aria-live="assertive"
        >
          {@error}
        </div>
      {/if}

      <!-- Preset ranges with visual indicators -->
      <div class="flex flex-wrap gap-2 mt-4 presets" role="radiogroup" aria-label="Date presets" phx-hook="PresetNavigation">
        {#for preset <- @presets}
          <button
            type="button"
            role="radio"
            class={[
              "preset-button px-3 py-1 rounded text-sm font-medium transition-all duration-[var(--duration-normal)]",
              if(preset_active?(preset, @start_date, @end_date),
                do: "bg-primary text-primary-foreground shadow",
                else: "bg-muted hover:bg-muted/80"
              )
            ]}
            phx-click="apply_preset"
            phx-value-preset={preset}
            aria-checked={preset_active?(preset, @start_date, @end_date)}
            aria-label={preset_label(preset)}
            tabindex={preset_active?(preset, @start_date, @end_date) && 0 || -1}
          >
            {preset_label(preset)}
          </button>
        {/for}
      </div>

      <!-- Apply button with keyboard navigation support -->
      <button
        type="button"
        class={[
          "apply-button mt-4 px-4 py-2 rounded font-medium transition-all duration-[var(--duration-normal)]",
          if(@error,
            do: "bg-muted text-muted-foreground cursor-not-allowed",
            else: "bg-primary text-primary-foreground hover:bg-primary/90"
          )
        ]}
        phx-click={@on_apply}
        disabled={@error != nil}
        aria-disabled={@error != nil}
        tabindex="0"
      >
        Apply
      </button>
    </div>
    """
  end

  # Keyboard navigation handlers
  def handle_event("keydown", %{"key" => "ArrowLeft"}, socket) do
    # Navigate to previous preset
    {:noreply, socket}
  end

  def handle_event("keydown", %{"key" => "ArrowRight"}, socket) do
    # Navigate to next preset
    {:noreply, socket}
  end

  def handle_event("keydown", %{"key" => "Enter"}, socket) do
    # Apply currently focused preset
    {:noreply, socket}
  end

  # Preset label translations
  defp preset_label(:today), do: "Today"
  defp preset_label(:this_week), do: "This Week"
  defp preset_label(:this_month), do: "This Month"

  # Check if preset is currently active
  defp preset_active?(:today, start, _end),
    do: Date.to_iso8601(start) == Date.to_iso8601(Date.utc_today())

  defp preset_active?(:this_week, start, _end) do
    today = Date.utc_today()
    start_of_week = Date.beginning_of_week(today)
    end_of_week = Date.end_of_week(today)
    Date.compare(start, start_of_week) != :lt and Date.compare(start, end_of_week) != :gt
  end

  defp preset_active?(:this_month, start, _end) do
    today = Date.utc_today()
    start_of_month = Date.beginning_of_month(today)
    end_of_month = Date.end_of_month(today)
    Date.compare(start, start_of_month) != :lt and Date.compare(start, end_of_month) != :gt
  end
end
