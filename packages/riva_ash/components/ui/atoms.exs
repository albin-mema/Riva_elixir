defmodule RivaAsh.Components.UI.Atoms do
  @moduledoc """
  Standardized prop structure and utilities for all atomic components.
  Enforces consistent naming, variants, and accessibility requirements.
  """

  @variants [:primary, :secondary, :tertiary]
  @sizes [:sm, :md, :lg]

  @doc """
  Standardizes props across all atoms with consistent naming and defaults.
  All atoms should use this to ensure prop consistency.

  ## Usage

      def button(assigns) do
        assigns = Atoms.standard_assigns(assigns, variant: :primary)
        # ...
      end
  """
  def standard_assigns(assigns, defaults \\ []) do
    assigns
    |> assign_new(:variant, fn -> validate_variant(defaults[:variant] || :primary) end)
    |> assign_new(:size, fn -> validate_size(defaults[:size] || :md) end)
    |> assign_new(:disabled, fn -> !!defaults[:disabled] end)
    |> assign_new(:aria_label, fn -> defaults[:aria_label] end)
  end

  @doc """
  Generates variant-specific classes using design tokens.
  """
  def variant_classes(:button, :primary) do
    "bg-primary text-primary-foreground hover:bg-primary/90 active:bg-primary/80"
  end

  def variant_classes(:button, :secondary) do
    "bg-secondary text-secondary-foreground hover:bg-secondary/90 active:bg-secondary/80"
  end

  def variant_classes(:button, :tertiary) do
    "bg-accent text-accent-foreground hover:bg-accent/90 active:bg-accent/80"
  end

  @doc """
  Generates size-specific classes.
  """
  def size_classes(:button, :sm) do
    "px-2 py-1 text-xs"
  end

  def size_classes(:button, :md) do
    "px-4 py-2 text-sm"
  end

  def size_classes(:button, :lg) do
    "px-6 py-3 text-base"
  end

  @doc """
  Generates focus-visible styles for accessibility.
  """
  def focus_styles do
    "focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
  end

  @doc """
  Generates ARIA attributes for accessibility compliance.
  """
  def aria_attributes(assigns) do
    %{
      "aria-disabled" => assigns.disabled,
      "aria-label" => assigns.aria_label
    }
    |> Enum.filter(fn {_, v} -> v end)
    |> Enum.into(%{})
  end

  defp validate_variant(variant) when variant in @variants, do: variant
  defp validate_variant(_), do: :primary

  defp validate_size(size) when size in @sizes, do: size
  defp validate_size(_), do: :md
end
