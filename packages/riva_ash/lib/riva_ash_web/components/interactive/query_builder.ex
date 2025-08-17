defmodule RivaAshWeb.Components.Interactive.QueryBuilder do
  @moduledoc """
  React Awesome Query Builder (RAQB) wrapper using live_react.

  This component mounts a React-based query builder inside LiveView using the
  LiveReact hook. It expects a React component named "QueryBuilder" to be
  available in your JS bundle and registered globally (e.g. `window.QueryBuilder`).

  JS setup is described in the usage notes below.

  Usage (after JS setup):

      <.query_builder id="search-qb" config={my_config} value={my_value} />

  Notes:
  - `config` must follow RAQB's config shape (see @react-awesome-query-builder docs)
  - `value` is the initial query tree/value (can be nil for empty)
  - For server-side change handling, expose a callback prop in your React
    implementation that pushes events back to LiveView (see instructions)
  """
  use Phoenix.Component
  alias RivaAshWeb.CoreComponents, as: Core

  @type assigns :: %{
          required(:id) => String.t(),
          optional(:config) => map(),
          optional(:value) => map() | nil,
          optional(:class) => String.t(),
          optional(:use_tokens) => boolean(),
          optional(:rest) => map()
        }

  @doc """
  Render the Query Builder mount point via LiveReact.
  """
  @spec query_builder(assigns()) :: Phoenix.LiveView.Rendered.t()
  attr :id, :string, required: true, doc: "Unique id for this instance"
  attr :config, :map, default: %{}, doc: "RAQB config (fields/operators/widgets)"
  attr :value, :map, default: nil, doc: "Initial RAQB value/tree"
  attr :class, :string, default: "", doc: "Extra CSS classes for container"
  attr :use_tokens, :boolean, default: false, doc: "Use app CSS tokens to theme MUI"
  attr :rest, :global

  def query_builder(assigns) do
    props = %{
      config: Map.get(assigns, :config, %{}),
      value: Map.get(assigns, :value, nil),
      useTokens: Map.get(assigns, :use_tokens, false)
    }

    assigns = assign(assigns, :props, props)

    ~H"""
    <div class={["query-builder", @class]} {@rest}>
      <Core.live_react_component id={@id <> "-react"} module="QueryBuilder" props={@props} />
    </div>
    """
  end

  @doc """
  Generic React mount point via LiveReact.
  Provide `id`, `module` (global window component name), and optional `props` map.
  """
  @spec react(map()) :: Phoenix.LiveView.Rendered.t()
  attr :id, :string, required: true
  attr :module, :string, required: true
  attr :props, :map, default: %{}
  attr :class, :string, default: ""
  attr :rest, :global
  def react(assigns) do
    ~H"""
    <div class={@class} {@rest}>
      <Core.live_react_component id={@id} module={@module} props={@props} />
    </div>
    """
  end
end
