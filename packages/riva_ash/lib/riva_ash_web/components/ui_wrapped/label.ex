defmodule RivaAshWeb.Components.UIWrapped.Label do
  @moduledoc """
  App-level Label wrapper around SaladUI.Label.
  
  Provides form labels with consistent styling and accessibility.
  """
  use Phoenix.Component

  @doc """
  Renders a label for form fields.
  """
  attr :for, :string, default: nil, doc: "HTML for attribute to link to form element"
  attr :required, :boolean, default: false, doc: "Whether the field is required"
  attr :disabled, :boolean, default: false, doc: "Whether the label is disabled"
  attr :class, :string, default: ""
  attr :rest, :global

  slot :inner_block, required: true

  def label(assigns) do
    ~H"""
    <label
      for={@for}
      class={[
        "text-sm font-medium leading-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70",
        @class
      ]}
      {@rest}
    >
      <%= if @required do %>
        <span class="text-destructive ml-1">*</span>
      <% end %>
      <%= render_slot(@inner_block) %>
    </label>
    """
  end
end