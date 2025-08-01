defmodule RivaAshWeb.Components.Atoms.Select do
  @moduledoc """
  Select dropdown component with search functionality and consistent styling.
  """
  use Phoenix.Component

  @doc """
  Renders a select dropdown.
  """
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:options, :list, default: [])
  attr(:prompt, :string, default: nil)
  attr(:multiple, :boolean, default: false)
  attr(:searchable, :boolean, default: false)
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def select(assigns) do
    assigns = assign(assigns, :select_class, select_class(assigns))

    ~H"""
    <select
      class={@select_class}
      disabled={@disabled}
      required={@required}
      {@rest}
    >
      <option :if={@prompt} value=""><%= @prompt %></option>
      <option :for={{label, value} <- @options} value={value}><%= label %></option>
    </select>
    """
  end

  defp select_class(assigns) do
    base =
      "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"

    size = size_classes(assigns.size)
    variant = variant_classes(assigns.variant)

    Enum.join([base, size, variant, assigns.class], " ")
  end

  defp size_classes(size) do
    case size do
      "sm" -> "h-8 px-2 text-xs"
      "md" -> "h-9 px-3 text-sm"
      "lg" -> "h-10 px-4 text-base"
      _ -> "h-9 px-3 text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus:ring-destructive"
      "success" -> "border-green-500 focus:ring-green-500"
      _ -> ""
    end
  end
end
