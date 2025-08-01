defmodule RivaAshWeb.Components.Organisms.ClientForm do
  @moduledoc """
  Client registration and edit form component.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Toggle

  @doc """
  Renders a client form for registration or editing.
  """
  attr(:form, :map, required: true)
  attr(:editing, :boolean, default: false)
  attr(:show_registration_fields, :boolean, default: true)
  attr(:on_submit, :string, required: true)
  attr(:on_change, :string, required: true)
  attr(:on_cancel, :string, required: true)
  attr(:loading, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def client_form(assigns) do
    ~H"""
    <!-- Client form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest}>
      <.form_field field={@form[:first_name]} label="First Name" required />
      <.form_field field={@form[:last_name]} label="Last Name" required />
      <.form_field field={@form[:email]} label="Email" type="email" />
      <.form_field field={@form[:phone]} label="Phone" type="tel" />
      
      <div :if={@show_registration_fields}>
        <.toggle field={@form[:is_registered]} label="Register as permanent client" />
        <.form_field :if={@form[:is_registered].value} field={@form[:password]} label="Password" type="password" />
      </div>
      
      <div>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Client", else: "Create Client" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end
end
