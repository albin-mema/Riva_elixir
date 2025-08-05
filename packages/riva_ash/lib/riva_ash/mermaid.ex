defmodule RivaAsh.Mermaid do
  @moduledoc """
  Generates Mermaid.js diagrams from Ash resources.
  """

  @doc """
  Generates a Mermaid ERD diagram from Ash resources.

  ## Examples

      iex> RivaAsh.Mermaid.generate_erd(RivaAsh.Domain)
  """
  @spec generate_erd(Ash.Domain.t()) :: {:ok, String.t()} | {:error, any()}
  def generate_erd(domain) when is_atom(domain) do
    try do
      resources = Ash.Domain.Info.resources(domain)
      mermaid_resources = process_resources_to_mermaid(resources)
      result = build_erd_diagram(mermaid_resources)
      {:ok, result}
    rescue
      error -> {:error, error}
    end
  end

  @spec process_resources_to_mermaid(list(Ash.Resource.t())) :: list(String.t())
  defp process_resources_to_mermaid(resources) do
    Enum.map(resources, fn resource ->
      case resource_to_mermaid(resource) do
        {:ok, result} -> result
        {:error, _} -> ""
      end
    end)
  end

  @spec build_erd_diagram(list(String.t())) :: String.t()
  defp build_erd_diagram(mermaid_resources) do
    """
    erDiagram
    #{Enum.join(mermaid_resources, "\n\n")}
    """
    |> String.trim()
  end

  @spec resource_to_mermaid(Ash.Resource.t()) :: {:ok, String.t()} | {:error, any()}
  defp resource_to_mermaid(resource) when is_atom(resource) do
    try do
      attributes = resource.__info__(:attributes)
      relationships = Ash.Resource.Info.relationships(resource)
      resource_name = extract_resource_name(resource)

      with {:ok, attributes_str} <- format_attributes(attributes),
           {:ok, relationships_str} <- format_relationships(resource_name, relationships) do
        result = build_resource_diagram(resource_name, attributes_str, relationships_str)
        {:ok, result}
      else
        {:error, error} -> {:error, error}
      end
    rescue
      error -> {:error, error}
    end
  end

  @spec extract_resource_name(atom()) :: String.t()
  defp extract_resource_name(resource) do
    resource |> Module.split() |> List.last()
  end

  @spec build_resource_diagram(String.t(), String.t(), String.t()) :: String.t()
  defp build_resource_diagram(resource_name, attributes_str, relationships_str) do
    """
    #{resource_name} {
      #{attributes_str}
      #{relationships_str}
    }
    """
    |> String.trim()
  end

  @spec format_attributes(list()) :: {:ok, String.t()} | {:error, any()}
  defp format_attributes(attributes) when is_list(attributes) do
    result =
      attributes
      |> Enum.map(&format_attribute/1)
      |> Enum.join("\n")

    {:ok, result}
  end

  @spec format_attribute({atom(), list()}) :: String.t()
  defp format_attribute({name, types}) do
    type = extract_attribute_type(types)
    "    #{name} #{type}"
  end

  @spec extract_attribute_type(list()) :: String.t()
  defp extract_attribute_type([t | _]), do: inspect(t)
  defp extract_attribute_type(_), do: "any"

  @spec format_relationships(String.t(), list()) :: {:ok, String.t()} | {:error, list()}
  defp format_relationships(resource_name, relationships) when is_binary(resource_name) and is_list(relationships) do
    relationship_results = Enum.map(relationships, &relationship_to_mermaid(resource_name, &1))
    errors = extract_relationship_errors(relationship_results)

    case Enum.empty?(errors) do
      true ->
        relationship_strings = extract_relationship_strings(relationship_results)
        {:ok, Enum.join(relationship_strings, "\n")}
      false ->
        {:error, errors}
    end
  end

  @spec extract_relationship_errors(list({:ok, String.t()} | {:error, any()})) :: list(any())
  defp extract_relationship_errors(relationship_results) do
    Enum.filter(relationship_results, &match?({:error, _}, &1))
  end

  @spec extract_relationship_strings(list({:ok, String.t()} | {:error, any()})) :: list(String.t())
  defp extract_relationship_strings(relationship_results) do
    Enum.map(relationship_results, fn {:ok, str} -> str end)
  end

  @spec relationship_to_mermaid(String.t(), map()) :: {:ok, String.t()} | {:error, any()}
  defp relationship_to_mermaid(source_resource, %{
         name: name,
         destination: destination,
         cardinality: cardinality,
         type: type
       }) when is_binary(source_resource) and is_binary(name) do
    with {:ok, dest_resource} <- format_destination_resource(destination),
         {:ok, relationship_line} <-
           format_relationship_line(source_resource, dest_resource, name, cardinality, type) do
      {:ok, relationship_line}
    else
      {:error, error} -> {:error, error}
    end
  end

  @spec format_destination_resource(atom() | String.t()) :: {:ok, String.t()} | {:error, any()}
  defp format_destination_resource(destination) when is_atom(destination) or is_binary(destination) do
    case destination do
      mod when is_atom(mod) -> {:ok, mod |> Module.split() |> List.last()}
      _ -> {:ok, "#{destination}"}
    end
  end

  @spec format_relationship_line(String.t(), String.t(), String.t(), atom(), atom()) :: {:ok, String.t()} | {:error, any()}
  defp format_relationship_line(source_resource, dest_resource, name, cardinality, type)
       when is_binary(source_resource) and is_binary(dest_resource) and is_binary(name) and is_atom(cardinality) and is_atom(type) do
    result = build_relationship_line(source_resource, dest_resource, name, cardinality, type)
    {:ok, result}
  end

  @spec build_relationship_line(String.t(), String.t(), String.t(), atom(), atom()) :: String.t()
  defp build_relationship_line(source_resource, dest_resource, name, cardinality, type) do
    case type do
      :belongs_to ->
        "    #{source_resource} ||--o{ #{dest_resource} : " <>
          if(cardinality == :one, do: "belongs_to", else: "has_many") <>
          "_#{name}"

      :has_many ->
        "    #{source_resource} ||--o{ #{dest_resource} : has_many_#{name}"

      :has_one ->
        "    #{source_resource} ||--|| #{dest_resource} : has_one_#{name}"

      :many_to_many ->
        "    #{source_resource} }o--o{ #{dest_resource} : many_to_many_#{name}"

      _ ->
        "    #{source_resource} -- #{dest_resource} : #{name} (#{inspect(type)})"
    end
  end
end
