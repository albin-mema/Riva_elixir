alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.HTML, as: HTML
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML.FormField, as: FormField

defmodule RivaAshWeb.Components.UI.Select do
  @moduledoc """
  Implements a select component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a select component using the design system.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :options, :list, default: []
  attr :prompt, :string, default: nil
  attr :multiple, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :required, :boolean, default: false
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  @spec select(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def select(assigns) do
    # Render select using functional composition
    assigns
    |> Map.put_new(:select_class, select_class(assigns))
    |> Map.put_new(:wrapper_class, build_wrapper_class(assigns.field))
    |> Map.put_new(:error_class, build_error_class(assigns.field))
    |> Map.put_new(:required_class, build_required_class(assigns.required))
    |> Map.put_new(:options_class, build_options_class(assigns.multiple))
    |> render_select_component()
  end

  # Private helper for select rendering
  @spec render_select_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_select_component(assigns) do
    ~H"""
    <div class={@wrapper_class}>
      <select
        class={@select_class}
        disabled={@disabled}
        required={@required}
        multiple={@multiple}
        {@rest}
      >
        <option :if={@prompt} value=""><%= @prompt %></option>
        <option :for={{label, value} <- @options} value={value} class={@options_class}><%= label %></option>
      </select>
      <%= if has_error?(@field) do %>
        <p class={@error_class}>
          <%= error_message(@field) %>
        </p>
      <% end %>
    </div>
    """
  end

  # Helper function to build wrapper classes
  @spec build_wrapper_class(Phoenix.HTML.FormField.t() | nil) :: String.t()
  defp build_wrapper_class(field) do
    class =
      if field do
        "relative w-full"
      else
        "w-full"
      end

    class
  end

  # Helper function to build error classes
  @spec build_error_class(Phoenix.HTML.FormField.t() | nil) :: String.t()
  defp build_error_class(field) do
    class =
      if has_error?(field) do
        "text-sm text-destructive mt-1"
      else
        "hidden"
      end

    class
  end

  # Helper function to build required classes
  @spec build_required_class(boolean()) :: String.t()
  defp build_required_class(required) do
    class =
      if required do
        "required"
      else
        ""
      end

    class
  end

  # Helper function to build options classes
  @spec build_options_class(boolean()) :: String.t()
  defp build_options_class(multiple) do
    class =
      if multiple do
        "py-1"
      else
        ""
      end

    class
  end

  # Helper function to check if field has errors
  @spec has_error?(Phoenix.HTML.FormField.t() | nil) :: boolean()
  defp has_error?(field) do
    field && field.errors && length(field.errors) > 0
  end

  # Helper function to get error message
  @spec error_message(Phoenix.HTML.FormField.t()) :: String.t()
  defp error_message(field) do
    field.errors |> List.first() |> elem(1)
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
      "sm" -> "h-9 px-2 text-xs"
      "lg" -> "h-11 px-4 text-base"
      _unmatchedunmatched -> "h-10 px-3 text-sm"
    end
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus:ring-destructive"
      "success" -> "border-[var(--chart-5)] focus:ring-[var(--chart-5)]"
      _unmatchedunmatched -> ""
    end
  end
end
