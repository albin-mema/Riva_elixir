defmodule RivaAshWeb.Components.Forms.LayoutForm do
  @moduledoc """
  Layout configuration form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select
  import RivaAshWeb.Components.Atoms.Toggle

  @doc """
  Renders a layout configuration form.
  """
  attr(:form, :map, required: true)
  attr(:plots, :list, default: [])
  attr(:editing, :boolean, default: false)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def layout_form(assigns) do
    ~H"""
    <!-- Layout form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:name]} label="Layout Name" required />
      <.form_field field={@form[:plot_id]} label="Plot" type="select" options={@plots} required />
      
      <div>
        <h3>Layout Type</h3>
        <.form_field field={@form[:layout_type]} label="Type" type="select" options={[
          {"Grid Layout", "grid"},
          {"Free Layout", "free"},
          {"Linear Layout", "linear"}
        ]} required />
      </div>
      
      <div :if={@form[:layout_type].value == "grid"}>
        <h3>Grid Configuration</h3>
        <.form_field field={@form[:grid_rows]} label="Number of Rows" type="number" min="1" max="50" />
        <.form_field field={@form[:grid_columns]} label="Number of Columns" type="number" min="1" max="50" />
      </div>
      
      <div>
        <h3>Dimensions</h3>
        <.form_field field={@form[:width]} label="Width (pixels)" type="number" />
        <.form_field field={@form[:height]} label="Height (pixels)" type="number" />
      </div>
      
      <div>
        <h3>Appearance</h3>
        <.form_field field={@form[:background_color]} label="Background Color" type="color" />
        <.form_field field={@form[:background_image_url]} label="Background Image URL" type="url" />
      </div>
      
      <div>
        <.toggle field={@form[:is_active]} label="Active Layout" />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Layout", else: "Create Layout" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
