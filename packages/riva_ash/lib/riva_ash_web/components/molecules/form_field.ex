defmodule RivaAshWeb.Components.Molecules.FormField do
  @moduledoc """
  FormField component that combines label, input, and error messages.
  A molecule component that provides a complete form field experience.
  """
  use Phoenix.Component
  alias RivaAshWeb.Components.UI.Text, as: UIText
  alias RivaAshWeb.Components.UI.Icon, as: UIIcon
  alias RivaAshWeb.Components.UI.Input, as: UIInput
  alias RivaAshWeb.Components.UI.Textarea, as: UITextarea
  alias RivaAshWeb.Components.UI.Select, as: UISelect
  alias RivaAshWeb.Components.UI.Checkbox, as: UICheckbox

  @doc """
  Renders a complete form field with label, input, helper text, and error messages.

  ## Examples

      <.form_field
        field={@form[:email]}
        label="Email Address"
        type="email"
        placeholder="user@example.com"
        helper_text="We'll never share your email"
        required={true}
      />

      <.form_field
        field={@form[:password]}
        label="Password"
        type="password"
        icon={:lock_closed}
      />
  """
  attr(:field, Phoenix.HTML.FormField, required: true)
  attr(:label, :string, default: nil)
  attr(:type, :string, default: "text")
  attr(:placeholder, :string, default: "")
  attr(:helper_text, :string, default: nil)
  attr(:required, :boolean, default: false)
  attr(:disabled, :boolean, default: false)
  attr(:readonly, :boolean, default: false)
  attr(:icon, :atom, default: nil)
  attr(:icon_position, :string, default: "left", values: ~w(left right))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def form_field(assigns) do
    ~H"""
    <div class={field_wrapper_class(@class)}>
      <%= if @label do %>
        <UIText.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </UIText.text>
      <% end %>

      <div class="relative">
        <%= if @icon && @icon_position == "left" do %>
          <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
            <UIIcon.icon name={@icon} size="sm" class="text-muted-foreground" />
          </div>
        <% end %>

        <UIInput.input
          type={@type}
          field={@field}
          placeholder={@placeholder}
          disabled={@disabled}
          readonly={@readonly}
          variant={if @field.errors != [], do: "error", else: "default"}
          class={if @icon, do: icon_padding_class(@icon_position), else: ""}
          {@rest}
        />

        <%= if @icon && @icon_position == "right" do %>
          <div class="absolute right-3 top-1/2 -translate-y-1/2 pointer-events-none">
            <UIIcon.icon name={@icon} size="sm" class="text-muted-foreground" />
          </div>
        <% end %>
      </div>

      <%= if @helper_text && @field.errors == [] do %>
        <UIText.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </UIText.text>
      <% end %>

      <.field_errors field={@field} />
    </div>
    """
  end

  @doc """
  Renders a textarea form field.
  """
  attr(:field, Phoenix.HTML.FormField, required: true)
  attr(:label, :string, default: nil)
  attr(:placeholder, :string, default: "")
  attr(:helper_text, :string, default: nil)
  attr(:required, :boolean, default: false)
  attr(:disabled, :boolean, default: false)
  attr(:readonly, :boolean, default: false)
  attr(:rows, :integer, default: 4)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def textarea_field(assigns) do
    ~H"""
    <div class={field_wrapper_class(@class)}>
      <%= if @label do %>
        <UIText.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </UIText.text>
      <% end %>

      <UITextarea.textarea
        field={@field}
        placeholder={@placeholder}
        disabled={@disabled}
        readonly={@readonly}
        rows={@rows}
        variant={if @field.errors != [], do: "error", else: "default"}
        {@rest}
      />

      <%= if @helper_text && @field.errors == [] do %>
        <UIText.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </UIText.text>
      <% end %>

      <.field_errors field={@field} />
    </div>
    """
  end

  @doc """
  Renders a select form field.
  """
  attr(:field, Phoenix.HTML.FormField, required: true)
  attr(:label, :string, default: nil)
  attr(:options, :list, required: true)
  attr(:prompt, :string, default: nil)
  attr(:helper_text, :string, default: nil)
  attr(:required, :boolean, default: false)
  attr(:disabled, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def select_field(assigns) do
    ~H"""
    <div class={field_wrapper_class(@class)}>
      <%= if @label do %>
        <UIText.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </UIText.text>
      <% end %>

      <UISelect.select
        field={@field}
        options={@options}
        prompt={@prompt}
        disabled={@disabled}
        variant={if @field.errors != [], do: "error", else: "default"}
        {@rest}
      />

      <%= if @helper_text && @field.errors == [] do %>
        <UIText.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </UIText.text>
      <% end %>

      <.field_errors field={@field} />
    </div>
    """
  end

  @doc """
  Renders a checkbox form field.
  """
  attr(:field, Phoenix.HTML.FormField, required: true)
  attr(:label, :string, required: true)
  attr(:helper_text, :string, default: nil)
  attr(:disabled, :boolean, default: false)
  attr(:class, :string, default: "")
  attr(:rest, :global)

  def checkbox_field(assigns) do
    ~H"""
    <div class={field_wrapper_class(@class)}>
      <UICheckbox.checkbox
        field={@field}
        label={@label}
        description={@helper_text}
        disabled={@disabled}
        variant={if @field.errors != [], do: "error", else: "default"}
        {@rest}
      />

      <.field_errors field={@field} />
    </div>
    """
  end

  defp field_wrapper_class(class) do
    "space-y-1 #{class}"
  end

  defp icon_padding_class(icon_position) do
    case icon_position do
      "left" -> "pl-10"
      "right" -> "pr-10"
      _ -> ""
    end
  end

  defp field_errors(assigns) do
    ~H"""
    <div :if={@field.errors != []} class="mt-1 space-y-1">
      <%= for error <- @field.errors do %>
        <div class="flex items-center gap-1">
          <UIIcon.icon name={:x_mark} size="xs" class="text-destructive" />
          <UIText.text variant="small" color="destructive">
            <%= translate_error(error) %>
          </UIText.text>
        </div>
      <% end %>
    </div>
    """
  end

  defp translate_error({msg, opts}) do
    # This is a simplified version. In a real app, you'd use Gettext
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", to_string(value))
    end)
  end

  defp translate_error(msg) when is_binary(msg), do: msg
end
