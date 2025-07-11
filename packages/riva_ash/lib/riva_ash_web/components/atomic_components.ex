defmodule RivaAshWeb.Components.AtomicComponents do
  @moduledoc """
  Central module that exports all atomic design components.

  This module provides a single import point for all atomic design components,
  organized by their atomic design level (atoms, molecules, organisms, templates).

  ## Usage

  To use all atomic components in a module:

      use RivaAshWeb.Components.AtomicComponents

  To import specific levels:

      import RivaAshWeb.Components.AtomicComponents, only: [atoms: 0]
      import RivaAshWeb.Components.AtomicComponents, only: [molecules: 0]
  """

  defmacro __using__(_opts) do
    quote do
      import RivaAshWeb.Components.AtomicComponents
      atoms()
      molecules()
      organisms()
    end
  end

  @doc """
  Imports all atom-level components.

  Atoms are the basic building blocks of the design system:
  - Icon: For displaying icons
  - Button: For user actions
  - Text: For typography
  - Badge: For labels and status indicators
  """
  defmacro atoms do
    quote do
      import RivaAshWeb.Components.Atoms.Icon
      import RivaAshWeb.Components.Atoms.Button
      import RivaAshWeb.Components.Atoms.Text
      import RivaAshWeb.Components.Atoms.Badge
    end
  end

  @doc """
  Imports all molecule-level components.

  Molecules are simple components made from atoms:
  - Card: For grouping content
  - FormField: For form inputs with labels and errors
  - EmptyState: For displaying when no data is available
  """
  defmacro molecules do
    quote do
      import RivaAshWeb.Components.Molecules.Card
      import RivaAshWeb.Components.Molecules.FormField
      import RivaAshWeb.Components.Molecules.EmptyState
    end
  end

  @doc """
  Imports all organism-level components.

  Organisms are complex components made from atoms and molecules:
  - PageHeader: For consistent page headers
  - BusinessForm: For creating/editing businesses
  - BusinessCard: For displaying business information
  """
  defmacro organisms do
    quote do
      import RivaAshWeb.Components.Organisms.PageHeader
      import RivaAshWeb.Components.Organisms.BusinessForm
      import RivaAshWeb.Components.Organisms.BusinessCard
    end
  end

  @doc """
  Imports template-level components.

  Templates are page-level layout components.
  Currently no templates are defined.
  """
  defmacro templates do
    quote do
      # Add template imports here as they are created
    end
  end
end
