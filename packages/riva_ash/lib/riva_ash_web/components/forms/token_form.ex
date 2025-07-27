defmodule RivaAshWeb.Components.Forms.TokenForm do
  @moduledoc """
  Form component for creating and editing API tokens using atomic design system.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Molecules.FormField
  import RivaAshWeb.Components.Molecules.Card
  import RivaAshWeb.Components.Molecules.NotificationToast
  import RivaAshWeb.Components.Atoms.Button
  import RivaAshWeb.Components.Atoms.Text

  @doc """
  Renders a form for creating or editing tokens.
  """
  attr :form, :any, required: true
  attr :editing, :boolean, default: false
  attr :loading, :boolean, default: false
  attr :on_submit, :string, required: true
  attr :on_change, :string, required: true
  attr :on_cancel, :string, required: true
  attr :users, :list, default: []

  def token_form(assigns) do
    ~H"""
    <.card variant="elevated">
      <:header>
        <.card_title><%= if @editing, do: "Edit Token", else: "Create New Token" %></.card_title>
      </:header>
      <:body>
        <.form for={@form} phx-change={@on_change} phx-submit={@on_submit} phx-reset={@on_cancel} class="space-y-6">
          <!-- Error Toast -->
          <.notification_toast
            :if={@form.errors != []}
            type="error"
            message={format_errors(@form.errors)}
            show={@form.errors != []}
            aria-live="assertive"
            aria-atomic="true"
          />

          <.form_field
            field={@form[:user_id]}
            label="User"
            type="select"
            options={Enum.map(@users, &{&1.email, &1.id})}
            prompt="Select a user"
            helper_text="Select the user this token belongs to."
          />

          <.form_field
            field={@form[:purpose]}
            label="Purpose"
            type="text"
            placeholder="API access, integration, etc."
            helper_text="Describe the purpose of this token."
          />

          <.form_field
            field={@form[:expires_at]}
            label="Expires At"
            type="datetime-local"
            helper_text="Set when this token should expire."
          />

          <div class="flex justify-end space-x-3 pt-4 border-t">
            <.button type="button" variant="outline" phx-click={@on_cancel} disabled={@loading}>
              Cancel
            </.button>
            <.button type="submit" loading={@loading}>
              <%= if @editing, do: "Update Token", else: "Create Token" %>
            </.button>
          </div>
        </.form>
      </:body>
    </.card>
    """
  end

  defp format_errors(errors) do
    errors
    |> Enum.map(fn {field, {message, _}} -> "#{field}: #{message}" end)
    |> Enum.join(", ")
  end
end
