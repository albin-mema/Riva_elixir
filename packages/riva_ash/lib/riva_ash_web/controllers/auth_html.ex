defmodule RivaAshWeb.AuthHTML do
  @moduledoc """
  HTML templates for authentication-related pages.

  Provides reusable HTML components and templates for:
  - Sign-in forms
  - Registration forms
  - Authentication error messages
  - Authentication-related layouts
  """

  use RivaAshWeb, :html

  # Embed authentication-related templates from the components directory
  embed_templates("../components/core/auth/*")
end
