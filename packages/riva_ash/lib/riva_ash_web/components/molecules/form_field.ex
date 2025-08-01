defmodule RivaAshWeb.Components.Molecules.FormField do
  @moduledoc """
  FormField component that combines label, input, and error messages.
  A molecule component that provides a complete form field experience.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Text
  import RivaAshWeb.Components.Atoms.Icon

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
        <.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </.text>
      <% end %>

      <div class="relative">
        <%= if @icon && @icon_position == "left" do %>
          <div class="absolute left-3 top-1/2 -translate-y-1/2 pointer-events-none">
            <.icon name={@icon} size="sm" class="text-muted-foreground" />
          </div>
        <% end %>

        <input
          type={@type}
          name={@field.name}
          id={@field.id}
          value={@field.value}
          placeholder={@placeholder}
          disabled={@disabled}
          readonly={@readonly}
          class={input_class(@icon, @icon_position, @field.errors != [])}
          {@rest}
        />

        <%= if @icon && @icon_position == "right" do %>
          <div class="absolute right-3 top-1/2 -translate-y-1/2 pointer-events-none">
            <.icon name={@icon} size="sm" class="text-muted-foreground" />
          </div>
        <% end %>
      </div>

      <%= if @helper_text && @field.errors == [] do %>
        <.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </.text>
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
        <.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </.text>
      <% end %>

      <textarea
        name={@field.name}
        id={@field.id}
        placeholder={@placeholder}
        disabled={@disabled}
        readonly={@readonly}
        rows={@rows}
        class={textarea_class(@field.errors != [])}
        {@rest}
      ><%= @field.value %></textarea>

      <%= if @helper_text && @field.errors == [] do %>
        <.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </.text>
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
        <.text variant="label" required={@required} class="mb-2">
          <%= @label %>
        </.text>
      <% end %>

      <select
        name={@field.name}
        id={@field.id}
        disabled={@disabled}
        class={select_class(@field.errors != [])}
        {@rest}
      >
        <%= if @prompt do %>
          <option value=""><%= @prompt %></option>
        <% end %>
        <%= for {label, value} <- @options do %>
          <option value={value} selected={value == @field.value}>
            <%= label %>
          </option>
        <% end %>
      </select>

      <%= if @helper_text && @field.errors == [] do %>
        <.text variant="small" color="muted" class="mt-1">
          <%= @helper_text %>
        </.text>
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
      <div class="flex items-start">
        <input
          type="checkbox"
          name={@field.name}
          id={@field.id}
          value="true"
          checked={@field.value == true || @field.value == "true"}
          disabled={@disabled}
          class={checkbox_class()}
          {@rest}
        />
        <div class="ml-3">
          <label for={@field.id} class="text-sm font-medium leading-6 text-foreground cursor-pointer">
            <%= @label %>
          </label>
          <%= if @helper_text do %>
            <.text variant="small" color="muted">
              <%= @helper_text %>
            </.text>
          <% end %>
        </div>
      </div>

      <.field_errors field={@field} />
    </div>
    """
  end

  defp field_wrapper_class(class) do
    "space-y-1 #{class}"
  end

  defp input_class(icon, icon_position, has_errors) do
    base =
      "flex h-9 w-full rounded-md border bg-transparent px-3 py-1 text-sm shadow-sm transition-colors file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"

    padding =
      cond do
        icon && icon_position == "left" -> "pl-10"
        icon && icon_position == "right" -> "pr-10"
        true -> ""
      end

    border =
      if has_errors do
        "border-destructive focus-visible:ring-destructive"
      else
        "border-input"
      end

    Enum.join([base, padding, border], " ")
  end

  defp textarea_class(has_errors) do
    base =
      "flex min-h-[60px] w-full rounded-md border bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"

    border =
      if has_errors do
        "border-destructive focus-visible:ring-destructive"
      else
        "border-input"
      end

    "#{base} #{border}"
  end

  defp select_class(has_errors) do
    base =
      "flex h-9 w-full items-center justify-between rounded-md border bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus:outline-none focus:ring-1 focus:ring-ring disabled:cursor-not-allowed disabled:opacity-50"

    border =
      if has_errors do
        "border-destructive focus:ring-destructive"
      else
        "border-input"
      end

    "#{base} #{border}"
  end

  defp checkbox_class do
    "h-4 w-4 rounded border-input text-primary focus:ring-2 focus:ring-ring focus:ring-offset-2 focus:ring-offset-background disabled:cursor-not-allowed disabled:opacity-50"
  end

  defp field_errors(assigns) do
    ~H"""
    <div :if={@field.errors != []} class="mt-1 space-y-1">
      <%= for error <- @field.errors do %>
        <div class="flex items-center gap-1">
          <.icon name={:exclamation_circle} size="xs" class="text-destructive" variant="mini" />
          <.text variant="small" color="destructive">
            <%= translate_error(error) %>
          </.text>
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
