defmodule RivaAshWeb.Layouts do
  @moduledoc """
  This module contains different layouts used by your application.

  See the `layouts` directory for all templates available.
  The "root" layout is a skeleton rendered around other layouts.
  The "app" layout is the default layout used by most pages.
  """
  use RivaAshWeb, :html

  import RivaAshWeb.CoreComponents

  embed_templates "layouts/*"
end
