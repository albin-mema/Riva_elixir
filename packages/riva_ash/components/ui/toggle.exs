defmodule RivaAsh.Components.UI.Toggle do
  @moduledoc """
  Toggle switch component with variant and size support.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  @doc """
  Renders a toggle switch.

  ## Props

    * `:variant` - The visual variant (primary, secondary, tertiary)
    * `:size` - The size of the toggle (sm, md, lg)
    * `:disabled` - Whether the toggle is disabled
    * `:aria_label` - Accessibility label for screen readers
    * `:checked` - Current toggle state (true/false)
    * `:on_change` - Function to call when toggle state changes
  """
  def toggle(assigns) do
    assigns = Atoms.standard_assigns(assigns, variant: :primary, size: :md, checked: false)

    ~H"""
    <div class="inline-flex relative items-center">
      <input
        type="checkbox"
        id={@id}
        checked={@checked}
        disabled={@disabled}
        class={["toggle-switch", variant_classes(:toggle, @variant), size_classes(:toggle, @size)] ++ ["#{@focus_styles}"]}
        {@aria_attributes.assigns}
        phx-change={@on_change}
      />
      <label for={@id} class="ml-2 text-sm cursor-pointer">
        <%= @label %>
      </label>
    </div>
    """
  end

  @doc """
  Generates variant-specific classes for toggles.
  """
  def variant_classes(:toggle, :primary) do
    "bg-primary text-primary-foreground hover:bg-primary/90 active:bg-primary/80"
  end

  def variant_classes(:toggle, :secondary) do
    "bg-secondary text-secondary-foreground hover:bg-secondary/90 active:bg-secondary/80"
  end

  def variant_classes(:toggle, :tertiary) do
    "bg-accent text-accent-foreground hover:bg-accent/90 active:bg-accent/80"
  end

  @doc """
  Generates size-specific classes for toggles.
  """
  def size_classes(:toggle, :sm) do
    "w-8 h-4 after:w-3 after:h-3 after:top-0.5 after:left-0.5 text-xs"
  end

  def size_classes(:toggle, :md) do
    "w-10 h-5 after:w-4 after:h-4 after:top-0.5 after:left-0.5 text-sm"
  end

  def size_classes(:toggle
