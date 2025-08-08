alias RivaAsh.Test, as: Test
alias Phoenix.LiveView, as: LiveView
alias Phoenix.LiveViewTest, as: LiveViewTest
alias Phoenix.LiveViewTest.View, as: View
alias MyAppWeb.Components.Badge, as: Badge
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAsh.Test.LiveViewHelpers do
  @moduledoc """
  Phoenix LiveView test helpers with data-testid ergonomics.

  Usage examples:

      import Phoenix.LiveViewTest
      import RivaAsh.Test.LiveViewHelpers

      {:ok, view, _html} = live(conn, ~p"/dashboard")

      # Assert an element exists (prefer data-testid)
      assert_has(view, "dashboard-title")
      assert_has(render(view), ~s([data-testid="dashboard-title"]))

      # Click by data-testid
      view = click_testid(view, "save-button")

      # Assert the view patched to a path
      assert_patch_to(view, ~p"/dashboard?tab=items")

      # Render function components directly
      html = render_component(&MyComponent.render/1, assigns)

  Dependencies:
  - Relies on Floki for HTML querying and Phoenix.LiveViewTest for actions.
  """

  import ExUnit.Assertions
  import Phoenix.LiveViewTest

  @doc """
  Assert that the given LiveView or HTML contains a selector.

  When `selector_or_testid` has no spaces or CSS punctuation, it's treated
  as a data-testid value and converted to `[data-testid="..."]`.
  Otherwise, it is treated as a CSS selector.
  """
  @spec assert_has(Phoenix.LiveView.t() | String.t(), String.t()) :: :ok
  def assert_has(view_or_html, selector_or_testid) when is_binary(selector_or_testid) do
    selector =
      if looks_like_testid?(selector_or_testid) do
        ~s([data-testid="#{selector_or_testid}"])
      else
        selector_or_testid
      end

    html =
      case view_or_html do
        %Phoenix.LiveView.Socket{} -> render(view_or_html)
        %Phoenix.LiveViewTest.View{} -> render(view_or_html)
        bin when is_binary(bin) -> bin
      end

    case Floki.find(html, selector) do
      [] ->
        flunk("""
        Expected to find selector:

            #{selector}

        In HTML:

            #{truncate_html(html)}
        """)

      _unmatchedunmatched ->
        :ok
    end
  end

  @doc """
  Click an element by its data-testid within the LiveView.

  Returns the updated view. Raises if the element is not found.
  """
  @spec click_testid(Phoenix.LiveViewTest.View.t(), String.t()) ::
          Phoenix.LiveViewTest.View.t()
  def click_testid(%Phoenix.LiveViewTest.View{} = view, testid) when is_binary(testid) do
    selector = ~s([data-testid="#{testid}"])
    _html = element(view, selector) |> render_click()
    view
  end

  @doc """
  Assert that the LiveView has patched to the expected path.
  """
  @spec assert_patch_to(Phoenix.LiveViewTest.View.t(), String.t()) :: :ok
  def assert_patch_to(%Phoenix.LiveViewTest.View{} = view, expected_path) when is_binary(expected_path) do
    assert_patch(view, expected_path)
    :ok
  end

  @doc """
  Convenience wrapper to render a function component with assigns.

  Example:

      render_component(&MyAppWeb.Components.Badge.badge/1, color: "primary", label: "Hi")

  """
  @spec render_component((map() -> Phoenix.LiveView.Rendered.t()), map() | keyword()) :: String.t()
  def render_component(fun, assigns) when is_function(fun, 1) do
    Phoenix.LiveViewTest.render_component(fun, Map.new(assigns))
  end

  # Internal utilities

  defp looks_like_testid?(val) do
    # Heuristic: treat simple tokens as testid, otherwise as CSS
    not String.contains?(val, [" ", "#", ".", "[", "]", ">", "+", "~", ":"])
  end

  defp truncate_html(html, max \\ 1_500) do
    if String.length(html) > max do
      String.slice(html, 0, max) <> "\n... (truncated)"
    else
      html
    end
  end
end
