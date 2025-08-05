defmodule RivaAshWeb.UIDemoHTML do
  @moduledoc """
  HTML module for UI demonstration templates.

  Provides template functions for rendering UI component demonstrations,
  including interactive examples, documentation, and showcase pages.

  Features:
  - Component showcase templates
  - Interactive demonstration layouts
  - Documentation rendering
  - Error page templates

  Uses functional programming patterns with proper documentation and
  follows Phoenix HTML best practices.
  """

  use RivaAshWeb, :html

  import RivaAshWeb.Components.UI
  import RivaAshWeb.CoreComponents, only: [icon: 1]

  @type assigns :: map()
  @type template_result :: {:safe, iodata()}

  @doc """
  Renders the main UI demo index page.

  Displays a comprehensive showcase of all available UI components
  organized by category with navigation and interactive features.

  ## Assigns
    - title: Page title
    - description: Page description
    - components: List of component categories
    - features: List of platform features
    - last_updated: Date of last update
  """
  @spec index(assigns()) :: template_result()
  def index(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50">
      <!-- Header -->
      <header class="bg-white shadow-sm border-b border-gray-200">
        <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div class="flex justify-between items-center py-6">
            <div>
              <h1 class="text-3xl font-bold text-gray-900">
                <%= @title %>
              </h1>
              <p class="mt-2 text-lg text-gray-600">
                <%= @description %>
              </p>
            </div>
            <div class="flex items-center space-x-4">
              <.link
                navigate="/admin"
                class="inline-flex items-center px-4 py-2 border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
              >
                <.icon name="hero-arrow-left" class="h-4 w-4 mr-2" />
                Back to Admin
              </.link>
            </div>
          </div>
        </div>
      </header>

      <!-- Main Content -->
      <main class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <!-- Features Section -->
        <section class="mb-12">
          <h2 class="text-2xl font-bold text-gray-900 mb-6">Platform Features</h2>
          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <.feature_card
              title="Responsive Design"
              description="Components adapt seamlessly to all screen sizes"
              icon="hero-device-phone-mobile"
            />
            <.feature_card
              title="Dark Mode"
              description="Built-in dark theme support for reduced eye strain"
              icon="hero-moon"
            />
            <.feature_card
              title="Accessibility"
              description="WCAG compliant components with keyboard navigation"
              icon="hero-eye"
            />
            <.feature_card
              title="Customizable"
              description="Easy theming and customization options"
              icon="hero-palette"
            />
            <.feature_card
              title="Interactive"
              description="Live examples with real-time interaction"
              icon="hero-hand-pointer"
            />
            <.feature_card
              title="Documented"
              description="Comprehensive documentation and API references"
              icon="hero-document-text"
            />
          </div>
        </section>

        <!-- Components Section -->
        <section>
          <h2 class="text-2xl font-bold text-gray-900 mb-6">Component Categories</h2>
          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            <%= for category <- @components do %>
              <.category_card
                name={category.name}
                title={category.title}
                description={category.description}
                count={category.count}
              />
            <% end %>
          </div>
        </section>

        <!-- Last Updated -->
        <div class="mt-12 text-center text-sm text-gray-500">
          Last updated: <%= @last_updated %>
        </div>
      </main>
    </div>
    """
  end

  @doc """
  Renders a specific component demo page.

  Shows detailed information, examples, and interactive preview
  for a specific UI component.

  ## Assigns
    - name: Component name
    - title: Component title
    - description: Component description
    - examples: List of example code snippets
    - props: List of component properties
    - events: List of component events
  """
  @spec show(assigns()) :: template_result()
  def show(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50">
      <!-- Header -->
      <header class="bg-white shadow-sm border-b border-gray-200">
        <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div class="flex justify-between items-center py-6">
            <div class="flex items-center">
              <.link
                navigate="/ui_demo"
                class="flex items-center text-gray-600 hover:text-gray-900 mr-6"
              >
                <.icon name="hero-arrow-left" class="h-4 w-4 mr-2" />
                Back to Components
              </.link>
              <div>
                <h1 class="text-3xl font-bold text-gray-900">
                  <%= @title %>
                </h1>
                <p class="mt-1 text-gray-600">
                  <%= @description %>
                </p>
              </div>
            </div>
          </div>
        </div>
      </header>

      <!-- Main Content -->
      <main class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div class="grid grid-cols-1 lg:grid-cols-3 gap-8">
          <!-- Examples Section -->
          <div class="lg:col-span-2">
            <h2 class="text-xl font-bold text-gray-900 mb-6">Examples</h2>
            <div class="space-y-8">
              <%= for example <- @examples do %>
                <.example_card title={example.title} description={example.description} code={example.code} />
              <% end %>
            </div>
          </div>

          <!-- Properties & Events Sidebar -->
          <div class="space-y-8">
            <!-- Properties -->
            <div class="bg-white rounded-lg shadow p-6">
              <h3 class="text-lg font-semibold text-gray-900 mb-4">Properties</h3>
              <div class="space-y-4">
                <%= for prop <- @props do %>
                  <.property_row name={prop.name} type={prop.type} default={prop.default} description={prop.description} />
                <% end %>
              </div>
            </div>

            <!-- Events -->
            <div class="bg-white rounded-lg shadow p-6">
              <h3 class="text-lg font-semibold text-gray-900 mb-4">Events</h3>
              <div class="space-y-4">
                <%= for event <- @events do %>
                  <.event_row name={event.name} description={event.description} />
                <% end %>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
    """
  end

  @doc """
  Renders an error page for UI demo.

  Displays user-friendly error messages with navigation options.

  ## Assigns
    - error: Error message or description
  """
  @spec error(assigns()) :: template_result()
  def error(assigns) do
    ~H"""
    <div class="min-h-screen bg-gray-50 flex items-center justify-center py-12 px-4 sm:px-6 lg:px-8">
      <div class="max-w-md w-full text-center">
        <div class="mx-auto h-12 w-12 text-red-400">
          <.icon name="hero-exclamation-triangle" class="h-12 w-12" />
        </div>
        <h2 class="mt-4 text-3xl font-extrabold text-gray-900">Oops!</h2>
        <p class="mt-2 text-gray-600">
          <%= @error %>
        </p>
        <div class="mt-6">
          <.link
            navigate="/ui_demo"
            class="inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
          >
            Browse Components
          </.link>
        </div>
      </div>
    </div>
    """
  end

  # Component helper functions

  defp feature_card(assigns) do
    ~H"""
    <div class="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow duration-200">
      <div class="flex items-center mb-4">
        <.icon name={@icon} class="h-8 w-8 text-indigo-600" />
        <h3 class="ml-3 text-lg font-semibold text-gray-900">
          <%= @title %>
        </h3>
      </div>
      <p class="text-gray-600">
        <%= @description %>
      </p>
    </div>
    """
  end

  defp category_card(assigns) do
    ~H"""
    <.link
      navigate={"/ui_demo/show?component_name=#{@name}"}
      class="block bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow duration-200 group"
    >
      <div class="flex items-center justify-between mb-4">
        <h3 class="text-lg font-semibold text-gray-900 group-hover:text-indigo-600 transition-colors">
          <%= @title %>
        </h3>
        <span class="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-indigo-100 text-indigo-800">
          <%= @count %> components
        </span>
      </div>
      <p class="text-gray-600 text-sm">
        <%= @description %>
      </p>
      <div class="mt-4 flex items-center text-indigo-600 group-hover:text-indigo-800 transition-colors">
        <span class="text-sm font-medium">Explore</span>
        <.icon name="arrow-right" class="ml-1 h-4 w-4" />
      </div>
    </.link>
    """
  end

  defp example_card(assigns) do
    ~H"""
    <div class="bg-white rounded-lg shadow-md overflow-hidden">
      <div class="px-6 py-4 border-b border-gray-200">
        <h3 class="text-lg font-semibold text-gray-900">
          <%= @title %>
        </h3>
        <p class="mt-1 text-gray-600">
          <%= @description %>
        </p>
      </div>
      <div class="bg-gray-900 p-6">
        <pre class="text-sm text-gray-100 overflow-x-auto"><code><%= @code %></code></pre>
      </div>
      <div class="px-6 py-4 bg-gray-50 border-t border-gray-200">
        <button class="inline-flex items-center px-3 py-1.5 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500">
          <.icon name="clipboard-copy" class="h-4 w-4 mr-2" />
          Copy Code
        </button>
      </div>
    </div>
    """
  end

  defp property_row(assigns) do
    ~H"""
    <div class="flex items-start">
      <dt class="text-sm font-medium text-gray-900 w-24">
        <%= @name %>
      </dt>
      <dd class="ml-3 flex-1">
        <div class="text-sm text-gray-600">
          <span class="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-blue-100 text-blue-800">
            <%= @type %>
          </span>
          <%= if @default do %>
            <span class="ml-2 text-gray-500">Default: <%= @default %></span>
          <% end %>
        </div>
        <p class="mt-1 text-sm text-gray-500">
          <%= @description %>
        </p>
      </dd>
    </div>
    """
  end

  defp event_row(assigns) do
    ~H"""
    <div class="flex items-start">
      <dt class="text-sm font-medium text-gray-900 w-24">
        <%= @name %>
      </dt>
      <dd class="ml-3 flex-1">
        <p class="text-sm text-gray-600">
          <%= @description %>
        </p>
      </dd>
    </div>
    """
  end
end
