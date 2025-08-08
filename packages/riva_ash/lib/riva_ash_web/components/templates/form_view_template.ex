alias RivaAshWeb.Components.Templates, as: Templates
alias RivaAshWeb.Components.Organisms, as: Organisms
alias RivaAshWeb.Components.Molecules, as: Molecules
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Templates.FormViewTemplate do
  @moduledoc """
  Form page template with validation and actions.
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Organisms.PageHeader
  import RivaAshWeb.Components.Molecules.Card

  @doc """
  Renders a form view template.
  """
  attr(:title, :string, required: true)
  attr(:description, :string, default: nil)
  attr(:form_title, :string, default: nil)
  attr(:show_progress, :boolean, default: false)
  attr(:current_step, :integer, default: 1)
  attr(:total_steps, :integer, default: 1)
  attr(:breadcrumbs, :list, default: [])
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:actions, required: false)
  slot(:form_content, required: true)
  slot(:sidebar_content, required: false)

  @spec form_view_template(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  def form_view_template(assigns) do
    # Render form view template using functional composition
    assigns
    |> Map.put_new(:template_class, build_template_class(assigns.class, assigns.variant))
    |> Map.put_new(:header_class, build_header_class(assigns.actions))
    |> Map.put_new(:title_class, build_title_class(assigns.title))
    |> Map.put_new(:description_class, build_description_class(assigns.description))
    |> Map.put_new(:actions_class, build_actions_class(assigns.actions))
    |> Map.put_new(:progress_class, build_progress_class(assigns.show_progress))
    |> Map.put_new(:progress_header_class, build_progress_header_class(assigns.show_progress))
    |> Map.put_new(:progress_bar_class, build_progress_bar_class(assigns.show_progress))
    |> Map.put_new(:layout_class, build_layout_class(assigns.sidebar_content))
    |> Map.put_new(:main_class, build_main_class(assigns.form_content))
    |> Map.put_new(:sidebar_class, build_sidebar_class(assigns.sidebar_content))
    |> render_form_view_template_component()
  end

  # Private helper for form view template rendering
  @spec render_form_view_template_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_form_view_template_component(assigns) do
    ~H"""
    <div class={@template_class} {@rest}>
      <.page_header title={@title} description={@description} class={@header_class}>
        <:action :for={action <- @actions}>
          <%= render_slot(action) %>
        </:action>
      </.page_header>

      <div :if={@show_progress} class={@progress_class}>
        <div class={@progress_header_class}>
          <span>Step <%= @current_step %> of <%= @total_steps %></span>
        </div>
        <div class={@progress_bar_class}>
          <div style={"width: #{@current_step / @total_steps * 100}%"}></div>
        </div>
      </div>

      <div class={@layout_class}>
        <main class={@main_class}>
          <.card>
            <:header :if={@form_title}>
              <h2><%= @form_title %></h2>
            </:header>
            <:body>
              <%= render_slot(@form_content) %>
            </:body>
          </.card>
        </main>

        <aside :if={@sidebar_content != []} class={@sidebar_class}>
          <%= render_slot(@sidebar_content) %>
        </aside>
      </div>
    </div>
    """
  end

  # Helper function to build template classes
  @spec build_template_class(String.t(), String.t()) :: String.t()
  defp build_template_class(class, variant) do
    base =
      case variant do
        "compact" -> "space-y-4"
        "card" -> "bg-card rounded-lg p-6 shadow-sm space-y-6"
        _unmatchedunmatched -> "space-y-6"
      end

    Enum.join([base, class], " ")
  end

  # Helper function to build header classes
  @spec build_header_class(list()) :: String.t()
  defp build_header_class(actions) do
    class =
      if actions != [] do
        "mb-6"
      else
        "mb-4"
      end

    class
  end

  # Helper function to build title classes
  @spec build_title_class(String.t()) :: String.t()
  defp build_title_class(title) do
    class =
      if title do
        "text-2xl font-bold"
      else
        "hidden"
      end

    class
  end

  # Helper function to build description classes
  @spec build_description_class(String.t() | nil) :: String.t()
  defp build_description_class(description) do
    class =
      if description do
        "text-muted-foreground"
      else
        "hidden"
      end

    class
  end

  # Helper function to build actions classes
  @spec build_actions_class(list()) :: String.t()
  defp build_actions_class(actions) do
    class =
      if actions != [] do
        "flex gap-2"
      else
        "hidden"
      end

    class
  end

  # Helper function to build progress container classes
  @spec build_progress_class(boolean()) :: String.t()
  defp build_progress_class(show_progress) do
    class =
      if show_progress do
        "form-progress mb-6"
      else
        "hidden"
      end

    class
  end

  # Helper function to build progress header classes
  @spec build_progress_header_class(boolean()) :: String.t()
  defp build_progress_header_class(show_progress) do
    class =
      if show_progress do
        "progress-header text-sm text-muted-foreground"
      else
        "hidden"
      end

    class
  end

  # Helper function to build progress bar classes
  @spec build_progress_bar_class(boolean()) :: String.t()
  defp build_progress_bar_class(show_progress) do
    class =
      if show_progress do
        "progress-bar w-full bg-muted rounded-full h-2"
      else
        "hidden"
      end

    class
  end

  # Helper function to build layout classes
  @spec build_layout_class(list()) :: String.t()
  defp build_layout_class(sidebar_content) do
    "form-layout grid grid-cols-1 gap-6 lg:grid-cols-3"
  end

  # Helper function to build main classes
  @spec build_main_class(list()) :: String.t()
  defp build_main_class(form_content) do
    class =
      if form_content != [] do
        "form-main lg:col-span-2"
      else
        "hidden"
      end

    class
  end

  # Helper function to build sidebar classes
  @spec build_sidebar_class(list()) :: String.t()
  defp build_sidebar_class(sidebar_content) do
    class =
      if sidebar_content != [] do
        "form-sidebar lg:col-span-1"
      else
        "hidden"
      end

    class
  end
end
