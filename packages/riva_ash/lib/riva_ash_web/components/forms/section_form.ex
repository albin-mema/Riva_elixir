defmodule RivaAshWeb.Components.Forms.SectionForm do
  @moduledoc """
  Section creation and editing form component using atomic design system.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Select

  @doc """
  Renders a section form.
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

  def section_form(assigns) do
    ~H"""
    <.card variant="elevated" {@rest}>
      <:body>
        <form phx-submit={@on_submit} phx-change={@on_change} class="space-y-6">
          <.form_field
            field={@form[:name]}
            label="Section Name"
            required
            helper_text="Enter a name for the section"
          />
          <.form_field
            field={@form[:description]}
            label="Description"
            type="textarea"
            helper_text="Provide a description for the section"
          />
          <.form_field
            field={@form[:plot_id]}
            label="Plot"
            type="select"
            options={plot_options(@plots)}
            required
            helper_text="Select the plot this section belongs to"
          />

          <div class="flex justify-end space-x-3 pt-4 border-t">
            <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
              Cancel
            </.button>
            <.button type="submit" loading={@loading}>
              <%= if @editing, do: "Update Section", else: "Create Section" %>
            </.button>
          </div>
        </form>
      </:body>
    </.card>
    """
  end

  defp plot_options(plots) do
    Enum.map(plots, fn plot ->
      {plot.name, plot.id}
    end)
  end
end
