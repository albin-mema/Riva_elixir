defmodule RivaAshWeb.Components.Forms.SectionForm do
  @moduledoc """
  Section creation and editing form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a section form.
  """
  attr :form, :map, required: true
  attr :plots, :list, default: []
  attr :editing, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def section_form(assigns) do
    ~H"""
    <!-- Section form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:name]} label="Section Name" required />
      <.form_field field={@form[:description]} label="Description" type="textarea" />
      <.form_field field={@form[:plot_id]} label="Plot" type="select" options={@plots} required />
      
      <div>
        <h3>Section Properties</h3>
        <.form_field field={@form[:capacity]} label="Maximum Capacity" type="number" />
        <.form_field field={@form[:area]} label="Area (sq ft)" type="number" />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Section", else: "Create Section" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
