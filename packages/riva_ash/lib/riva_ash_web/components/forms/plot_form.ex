defmodule RivaAshWeb.Components.Forms.PlotForm do
  @moduledoc """
  Plot creation and editing form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a plot form.
  """
  attr :form, :map, required: true
  attr :businesses, :list, default: []
  attr :editing, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :loading, :boolean, default: false
  attr :class, :string, default: ""
  attr :rest, :global

  def plot_form(assigns) do
    ~H"""
    <!-- Plot form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:name]} label="Plot Name" required />
      <.form_field field={@form[:description]} label="Description" type="textarea" />
      <.form_field field={@form[:business_id]} label="Business" type="select" options={@businesses} required />
      
      <.form_field field={@form[:address]} label="Address" type="textarea" />
      <.form_field field={@form[:total_area]} label="Total Area (sq ft)" type="number" />
      
      <div>
        <h3>Location Details</h3>
        <.form_field field={@form[:latitude]} label="Latitude" type="number" step="any" />
        <.form_field field={@form[:longitude]} label="Longitude" type="number" step="any" />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Plot", else: "Create Plot" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
