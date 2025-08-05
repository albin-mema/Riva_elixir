defmodule RivaAshWeb.LayoutHook do
  @moduledoc """
  A module that provides layout handling for LiveViews.
  
  This module ensures that authenticated LiveViews use the appropriate
  layout by automatically setting the layout attribute during compilation.
  It provides a clean way to manage layout consistency across the
  application's LiveView components.
  """

  @type opts :: keyword()
  @type module :: atom()

  @doc """
  Macro hook that sets up automatic layout assignment for LiveViews.
  
  When used in a LiveView module, this macro automatically sets the
  authenticated layout as the default, ensuring consistent UI structure
  across all authenticated views.
  
  ## Parameters
    - `_opts`: Configuration options (currently unused but reserved for future)
  
  ## Usage
      defmodule MyLiveView do
        use RivaAshWeb, :live_view
        use RivaAshWeb.LayoutHook
        # Layout will be automatically set to {RivaAshWeb.Layouts, :authenticated}
      end
  """
  @spec __using__(opts()) :: Macro.t()
  defmacro __using__(_opts) do
    quote do
      # Set the layout using the @layout module attribute
      @layout {RivaAshWeb.Layouts, :authenticated}
    end
  end

  @doc """
  Compilation hook that ensures layout consistency.
  
  This macro runs before module compilation and ensures that if no
  explicit layout is set, the authenticated layout is used as a fallback.
  This prevents accidental layout omissions and maintains UI consistency.
  
  ## Parameters
    - `_env`: The compilation environment (unused but required by the hook)
  
  ## Behavior
    - Checks if the module has an explicit @layout attribute
    - If no layout is set, automatically assigns the authenticated layout
    - This ensures all LiveViews have a consistent layout structure
  """
  @spec __before_compile__(map()) :: Macro.t()
  defmacro __before_compile__(_env) do
    quote do
      # Ensure the layout is set if not already specified
      if !Module.has_attribute?(__MODULE__, :layout) do
        @layout {RivaAshWeb.Layouts, :authenticated}
      end
    end
  end

  @doc """
  Validates that a module has properly configured layout settings.
  
  Provides runtime validation to ensure that LiveView modules have
  appropriate layout configuration for proper rendering.
  
  ## Parameters
    - `module`: The module to validate for layout configuration
  
  ## Returns
    `:ok` if layout is properly configured
    `{:error, reason}` if layout configuration is missing or invalid
  """
  @spec validate_layout(module()) :: :ok | {:error, String.t()}
  def validate_layout(module) do
    if module_module_info(module)[:attributes][:layout] do
      :ok
    else
      {:error, "Module #{module} missing layout configuration"}
    end
  end

  # Private helper function to safely get module information
  defp module_module_info(module) do
    try do
      module.module_info()
    rescue
      ArgumentError -> %{}
    end
  end

  @doc """
  Retrieves the configured layout for a given module.
  
  Extracts the layout configuration from a module's attributes
  for use in dynamic layout resolution or debugging.
  
  ## Parameters
    - `module`: The module to retrieve layout configuration from
  
  ## Returns
    The layout tuple `{layout_module, layout_function}` or `nil` if not configured
  """
  @spec get_layout(module()) :: {module(), atom()} | nil
  def get_layout(module) do
    case module_module_info(module)[:attributes][:layout] do
      [{layout, function}] when is_atom(layout) and is_atom(function) ->
        {layout, function}
      _ ->
        nil
    end
  end
end
