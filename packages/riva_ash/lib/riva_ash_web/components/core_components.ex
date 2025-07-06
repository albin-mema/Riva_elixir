defmodule RivaAshWeb.CoreComponents do
  @moduledoc """
  Provides core UI components.

  At first glance, this module may seem daunting, but its goal is to provide
  core building blocks for your application. The components in this module are
  designed to be used throughout your application.
  """
  use RivaAshWeb, :html

  import RivaAshWeb.Gettext
  alias Phoenix.LiveView.JS

  @doc """
  Renders flash notices.

  ## Examples

      <.flash kind={:info} flash={@flash} />
      <.flash kind={:info} phx-mounted={show("#flash")}>Welcome Back!</.flash>
  """
  attr :id, :string, doc: "the optional id of flash container"
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :rest, :global, doc: "the arbitrary HTML attributes to add to the flash container"

  slot :inner_block, doc: "the optional inner block that renders the flash message"

  def flash(assigns) do
    ~H"""
    <div
      :if={msg = render_slot(@inner_block) || Phoenix.Flash.get(@flash, @kind)}
      id={@id}
      phx-click={JS.push("lv:clear-flash", value: %{key: @kind}) |> hide("##{@id}")}
      role="alert"
      class={[
        "fixed top-2 right-2 mr-2 w-80 sm:w-96 z-50 rounded-lg p-3 ring-1",
        @kind == :info && "bg-emerald-50 text-emerald-800 ring-emerald-500 fill-cyan-900",
        @kind == :error && "bg-rose-50 text-rose-900 shadow-md ring-rose-500 fill-rose-900"
      ]}
      {@rest}
    >
      <p :if={@title} class="flex items-center gap-1.5 font-semibold text-sm leading-6">
        <.icon :if={@kind == :info} name="hero-information-circle-mini" class="w-4 h-4" />
        <.icon :if={@kind == :error} name="hero-exclamation-circle-mini" class="w-4 h-4" />
        <%= @title %>
      </p>
      <p class="mt-2 text-sm leading-5"><%= msg %></p>
      <button type="button" class="group top-1 right-1 absolute p-2" aria-label="close">
        <.icon name="hero-x-mark-solid" class="opacity-40 group-hover:opacity-70 w-5 h-5" />
      </button>
    </div>
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} />
  """
  attr :flash, :map, required: true, doc: "the map of flash messages"
  attr :id, :string, default: "flash-group", doc: "the optional id of flash container"

  def flash_group(assigns) do
    ~H"""
    <div id={@id}>
      <.flash kind={:info} title="Success!" flash={@flash} id="flash-info" />
      <.flash kind={:error} title="Error!" flash={@flash} id="flash-error" />
    </div>
    """
  end

  @doc """
  Renders a simple icon.
  """
  attr :name, :string, required: true
  attr :class, :string, default: nil

  def icon(%{name: "hero-" <> _} = assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  defp hide(js \\ %JS{}, selector) do
    JS.hide(js,
      to: selector,
      time: 200,
      transition:
        {"transition-all transform ease-out duration-200",
         "opacity-100 translate-y-0 sm:translate-x-0", "opacity-0 translate-y-2 sm:translate-x-2"}
    )
  end

  defp show(js \\ %JS{}, selector) do
    JS.show(js,
      to: selector,
      time: 200,
      transition:
        {"transition-all transform ease-out duration-200",
         "opacity-0 translate-y-2 sm:translate-x-2", "opacity-100 translate-y-0 sm:translate-x-0"}
    )
  end

  @doc """
  Renders a React component using live_react.
  """
  attr :module, :string, required: true
  attr :props, :map, default: %{}
  attr :id, :string, required: true

  def live_react_component(assigns) do
    ~H"""
    <div
      id={@id}
      phx-hook="LiveReact"
      data-live-react-class={@module}
      data-live-react-props={Jason.encode!(@props)}
    >
    </div>
    """
  end

end
