defmodule RivaAshWeb.LayoutHook do
  @moduledoc """
  A module that provides layout handling for LiveViews.
  It checks for an @layout module attribute and uses it if present,
  otherwise falls back to the default layout.
  """

  defmacro __before_compile__(_env) do
    quote do
      @layout Module.get_attribute(__MODULE__, :layout, {RivaAshWeb.Layouts, :authenticated})
    end
  end
end
