defmodule RivaAshWeb.LayoutHook do
  @moduledoc """
  A module that provides layout handling for LiveViews.
  It ensures that authenticated LiveViews use the authenticated layout.
  """

  defmacro __using__(_opts) do
    quote do
      # Set the layout using the @layout module attribute
      @layout {RivaAshWeb.Layouts, :authenticated}
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      # Ensure the layout is set if not already specified
      unless Module.has_attribute?(__MODULE__, :layout) do
        @layout {RivaAshWeb.Layouts, :authenticated}
      end
    end
  end
end
