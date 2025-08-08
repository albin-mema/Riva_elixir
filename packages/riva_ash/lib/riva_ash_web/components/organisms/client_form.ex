alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

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

  @spec client_form(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def client_form(assigns) do
    # Render client form using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class))
    |> Map.put_new(:form_class, build_form_class())
    |> Map.put_new(:registration_class, build_registration_class(assigns.show_registration_fields))
    |> Map.put_new(:actions_class, build_actions_class())
    |> render_client_form_component()
  end

  # Private helper for client form rendering
  @spec render_client_form_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_client_form_component(assigns) do
    ~H"""
    <!-- Client form implementation will go here -->
    <form phx-submit={@on_submit} phx-change={@on_change} {@rest} class={@form_class}>
      <.form_field field={@form[:first_name]} label="First Name" required />
      <.form_field field={@form[:last_name]} label="Last Name" required />
      <.form_field field={@form[:email]} label="Email" type="email" />
      <.form_field field={@form[:phone]} label="Phone" type="tel" />

      <div class={@registration_class}>
        <.toggle field={@form[:is_registered]} label="Register as permanent client" />
        <.form_field :if={@form[:is_registered].value} field={@form[:password]} label="Password" type="password" />
      </div>

      <div class={@actions_class}>
        <.button type="submit" loading={@loading}>
          <%= if @editing, do: "Update Client", else: "Create Client" %>
        </.button>
        <.button type="button" variant="outline" phx-click={@on_cancel}>Cancel</.button>
      </div>
    </form>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t()) :: String.t()
  defp build_container_class(class) do
    class
  end

  # Helper function to build form classes
  @spec build_form_class() :: String.t()
  defp build_form_class, do: ""

  # Helper function to build registration classes
  @spec build_registration_class(boolean()) :: String.t()
  defp build_registration_class(show_registration_fields) do
    if show_registration_fields, do: "", else: "hidden"
  end

  # Helper function to build actions classes
  @spec build_actions_class() :: String.t()
  defp build_actions_class, do: ""
end
