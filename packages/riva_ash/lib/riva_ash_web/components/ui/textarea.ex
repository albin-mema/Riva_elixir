alias RivaAshWeb.Components.UI, as: UI
alias Phoenix.HTML, as: HTML
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML.FormField, as: FormField

defmodule RivaAshWeb.Components.UI.Textarea do
  @moduledoc """
  Implements a textarea component using the design system.
  """
  use Phoenix.Component

  @doc """
  Renders a textarea component using the design system.
  """
  attr :field, Phoenix.HTML.FormField, default: nil
  attr :value, :string, default: nil
  attr :placeholder, :string, default: ""
  attr :disabled, :boolean, default: false
  attr :readonly, :boolean, default: false
  attr :required, :boolean, default: false
  attr :rows, :integer, default: 3
  attr :variant, :string, default: "default", values: ~w(default error success)
  attr :size, :string, default: "default", values: ~w(default sm lg)
  attr :class, :string, default: ""
  attr :rest, :global

  @spec textarea(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def textarea(assigns) do
    # Render textarea using functional composition
    assigns
    |> handle_form_field_integration()
    |> Map.put_new(:textarea_class, textarea_class(assigns))
    |> Map.put_new(:wrapper_class, build_wrapper_class(assigns.field))
    |> Map.put_new(:error_class, build_error_class(assigns.field))
    |> Map.put_new(:required_class, build_required_class(assigns.required))
    |> Map.put_new(:rows_class, build_rows_class(assigns.rows))
    |> render_textarea_component()
  end

  # Private helper for textarea rendering
  @spec render_textarea_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_textarea_component(assigns) do
    ~H"""
    <div class={@wrapper_class}>
      <textarea
        class={@textarea_class}
        placeholder={@placeholder}
        disabled={@disabled}
        readonly={@readonly}
        required={@required}
        rows={@rows}
        {@rest}
      ><%= @value %></textarea>
      <%= if has_error?(@field) do %>
        <p class={@error_class}>
          <%= error_message(@field) %>
        </p>
      <% end %>
    </div>
    """
  end

  # Helper function to handle form field integration
  @spec handle_form_field_integration(map()) :: map()
  defp handle_form_field_integration(assigns) do
    case assigns.field do
      nil -> assigns
      field -> Map.put(assigns, :value, assigns.value || field.value)
    end
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

  # Helper function to build rows classes
  @spec build_rows_class(integer()) :: String.t()
  defp build_rows_class(rows) do
    case rows do
      1 -> "h-20"
      2 -> "h-32"
      3 -> "h-48"
      4 -> "h-64"
      5 -> "h-80"
      _unmatchedunmatched -> "h-48"
    end
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

  defp textarea_class(assigns) do
    base =
      "flex w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"

    variant = variant_classes(assigns.variant)
    size = size_classes(assigns.size)

    Enum.join([base, variant, size, assigns.class], " ")
  end

  defp variant_classes(variant) do
    case variant do
      "default" -> ""
      "error" -> "border-destructive focus-visible:ring-destructive"
      "success" -> "border-[var(--chart-5)] focus-visible:ring-[var(--chart-5)]"
      _unmatchedunmatched -> ""
    end
  end

  defp size_classes(size) do
    case size do
      "sm" -> "text-xs"
      "lg" -> "text-base"
      _unmatchedunmatched -> "text-sm"
    end
  end
end
