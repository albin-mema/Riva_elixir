if Mix.env() == :dev do
  defmodule RivaAshWeb.Storybook do
    @moduledoc """
    Phoenix Storybook configuration for component development and testing.
    
    This module configures the interactive Storybook environment for developing
    and documenting UI components. It provides a sandboxed environment for
    component testing with proper asset loading and styling isolation.
    
    Storybook is only available in the development environment for security
    and performance reasons.
    """

    use PhoenixStorybook,
      otp_app: :riva_ash_web,
      content_path: Path.expand("../../storybook", __DIR__),
      # assets path are remote path, not local file-system paths
      css_path: "/assets/storybook.css",
      js_path: "/assets/storybook.js",
      sandbox_class: "riva-ash-web"

    @type config :: keyword()
    @type story_id :: String.t()
    @type category :: String.t()

    @doc """
    Returns the base configuration for the Storybook.
    
    Provides access to the underlying Storybook configuration for
    custom integrations and extensions.
    """
    @spec base_config() :: config()
    def base_config do
      [
        otp_app: :riva_ash_web,
        content_path: Path.expand("../../storybook", __DIR__),
        css_path: "/assets/storybook.css",
        js_path: "/assets/storybook.js",
        sandbox_class: "riva-ash-web"
      ]
    end

    @doc """
    Validates the Storybook configuration for common issues.
    
    Performs runtime validation to ensure all required paths exist
    and configuration is properly set up for component development.
    """
    @spec validate_config() :: :ok | {:error, String.t()}
    def validate_config do
      with :ok <- validate_content_path(),
           :ok <- validate_asset_paths() do
        :ok
      else
        {:error, reason} -> {:error, reason}
      end
    end

    @doc """
    Returns the list of available component categories.
    
    Organizes components into logical categories for better navigation
    and discoverability in the Storybook interface.
    """
    @spec categories() :: [category()]
    def categories do
      [
        "Atoms",
        "Molecules",
        "Organisms",
        "Templates",
        "Business Components",
        "UI Components"
      ]
    end

    @doc """
    Checks if a specific story is available in the Storybook.
    
    Provides programmatic access to story availability for
    dynamic content and conditional rendering.
    """
    @spec story_available?(story_id()) :: boolean()
    def story_available?(story_id) do
      # Implementation would check if the story exists in the content path
      # This is a placeholder for actual story discovery logic
      true
    end

    # Private helper functions
    defp validate_content_path do
      content_path = Path.expand("../../storybook", __DIR__)
      if File.exists?(content_path) do
        :ok
      else
        {:error, "Storybook content path not found: #{content_path}"}
      end
    end

    defp validate_asset_paths do
      # In a real implementation, you'd check if the assets exist
      # or if the paths are properly configured
      :ok
    end
  end
end
