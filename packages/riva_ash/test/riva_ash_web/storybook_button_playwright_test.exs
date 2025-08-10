defmodule RivaAshWeb.StorybookButtonPlaywrightTest do
  use RivaAshWeb.FeatureCase, async: false
  use PhoenixTest.Playwright.Case
  import PhoenixTest

  @moduletag browser: :chromium

  test "button storybook stub renders", %{conn: conn} do
    conn
    |> visit("/storybook/ui/button")
    # Robust role-based selector for the heading
    |> assert_has(~s(role=heading[name="UI.Button Demo"]))
    # Smoke the presence of a couple of rendered buttons
    |> assert_has(~s(role=button[name="Default"]))
    |> assert_has(~s(role=button[name="Secondary"]))
  end
end
