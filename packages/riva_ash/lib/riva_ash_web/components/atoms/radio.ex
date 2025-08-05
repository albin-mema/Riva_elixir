defmodule RivaAshWeb.Components.Atoms.Radio do
  @moduledoc """
  Radio button component with groups and validation states.
  """
  use Phoenix.Component

  @doc """
  Renders a radio button input.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :string, required: true)
  attr(:checked, :boolean, default: false)
  attr(:label, :string, default: nil)
  attr(:description, :string, default: nil)
  attr(:disabled, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def radio(assigns) when is_map(assigns) do
    assigns
    |> validate_required_attributes()
    |> render_radio()
  end

  defp validate_required_attributes(assigns) when is_map(assigns) do
    case Map.get(assigns, :value) do
      nil -> raise ArgumentError, "value is required for radio component"
      _ -> assigns
    end
  end

  defp render_radio(assigns) do
    ~H"""
    <!-- Radio implementation will go here -->
    <div>
      <input type="radio" {@rest} />
      <label :if={@label}><%= @label %></label>
    </div>
    """
  end

  @doc """
  Renders a radio group.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:options, :list, required: true)
  attr(:label, :string, default: nil)
  attr(:orientation, :string, default: "vertical", values: ~w(horizontal vertical))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def radio_group(assigns) when is_map(assigns) do
    assigns
    |> validate_radio_group_attributes()
    |> render_radio_group()
  end

  defp validate_radio_group_attributes(assigns) when is_map(assigns) do
    case Map.get(assigns, :options) do
      nil -> raise ArgumentError, "options are required for radio_group component"
      [] -> raise ArgumentError, "options cannot be empty for radio_group component"
      _ -> assigns
    end
  end

  defp render_radio_group(assigns) do
    ~H"""
    <!-- Radio group implementation will go here -->
    <div {@rest}>
      <label :if={@label}><%= @label %></label>
      <div>
        <.radio :for={{label, value} <- @options} value={value} label={label} />
      </div>
    </div>
    """
  end
end
