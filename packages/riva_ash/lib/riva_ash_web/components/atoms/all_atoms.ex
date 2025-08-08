alias RivaAshWeb.Components.Atoms, as: Atoms
alias RivaAshWeb.Components.UI, as: UI

defmodule RivaAshWeb.Components.Atoms.AllAtoms do
  @moduledoc """
  This module serves as a central point for importing all atomic components.

  Follows functional core, imperative shell pattern with comprehensive type safety.
  """
  use Phoenix.Component

  @type assigns :: map()
  @type import_result :: :ok | {:error, String.t()}

  @doc """
  Imports all atomic components with validation.

  ## Examples

      use RivaAshWeb.Components.Atoms.AllAtoms
  """
  @spec __using__(opts :: keyword()) :: Macro.t()
  defmacro __using__(opts) do
    # Validate options using pattern matching
    validate_opts!(opts)

    quote do
      import RivaAshWeb.Components.Atoms.AllAtoms

      # Import individual atom components with proper organization
      import RivaAshWeb.Components.Atoms.Avatar
      import RivaAshWeb.Components.Atoms.Badge
      import RivaAshWeb.Components.Atoms.Button
      import RivaAshWeb.Components.Atoms.Checkbox
      import RivaAshWeb.Components.Atoms.DatePicker
      import RivaAshWeb.Components.Atoms.Icon
      import RivaAshWeb.Components.Atoms.Input
      import RivaAshWeb.Components.Atoms.Radio
      import RivaAshWeb.Components.Atoms.Select
      import RivaAshWeb.Components.Atoms.Spinner
      import RivaAshWeb.Components.Atoms.Text
      import RivaAshWeb.Components.Atoms.TextInput
      import RivaAshWeb.Components.Atoms.TimePicker
      import RivaAshWeb.Components.Atoms.Toggle
      import RivaAshWeb.Components.Atoms.Tooltip

      # Import UI components for canonical usage
      alias RivaAshWeb.Components.UI.Button, as: UIButton
      alias RivaAshWeb.Components.UI.Input, as: UIInput
      alias RivaAshWeb.Components.UI.Text, as: UIText
      alias RivaAshWeb.Components.UI.Icon, as: UIIcon
      alias RivaAshWeb.Components.UI.Badge, as: UIBadge
      alias RivaAshWeb.Components.UI.Card, as: UICard
      alias RivaAshWeb.Components.UI.Select, as: UISelect
      alias RivaAshWeb.Components.UI.Spinner, as: UISpinner
    end
  end

  # Private helper functions with type specifications

  @doc """
  Validates macro options with early return pattern.

  ## Parameters
  - opts: Keyword list of options

  ## Returns
  - :ok if valid

  ## Raises
  - ArgumentError for invalid options
  """
  @spec validate_opts!(keyword()) :: :ok
  defp validate_opts!(opts) do
    opts
    |> Keyword.keys()
    |> Enum.each(fn key ->
      case key do
        :only -> :ok
        :except -> :ok
        _ -> raise ArgumentError, "Invalid option #{inspect(key)} for AllAtoms"
      end
    end)
  end
end
