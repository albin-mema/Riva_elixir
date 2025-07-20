defmodule RivaAsh.Mermaid do
  @moduledoc """
  Generates Mermaid.js diagrams from Ash resources.
  """

  @doc """
  Generates a Mermaid ERD diagram from Ash resources.

  ## Examples

      iex> RivaAsh.Mermaid.generate_erd(RivaAsh.Domain)
  """
  def generate_erd(domain) do
    try do
      resources = Ash.Domain.Info.resources(domain)
      mermaid_resources = Enum.map(resources, fn resource ->
        case resource_to_mermaid(resource) do
          {:ok, result} -> result
          {:error, _} -> ""
        end
      end)

      result = """
      erDiagram
      #{Enum.join(mermaid_resources, "\n\n")}
      """
      |> String.trim()

      {:ok, result}
    rescue
      error -> {:error, error}
    end
  end

  defp resource_to_mermaid(resource) do
    try do
      attributes = resource.__info__(:attributes)
      relationships = Ash.Resource.Info.relationships(resource)
      resource_name = resource |> Module.split() |> List.last()

      case format_attributes(attributes) do
        {:ok, attributes_str} ->
          case format_relationships(resource_name, relationships) do
            {:ok, relationships_str} ->
              result = """
              #{resource_name} {
                #{attributes_str}
                #{relationships_str}
              }
              """
              |> String.trim()
              {:ok, result}
            {:error, error} -> {:error, error}
          end
        {:error, error} -> {:error, error}
      end
    rescue
      error -> {:error, error}
    end
  end

  defp format_attributes(attributes) do
    result = attributes
    |> Enum.map(fn {name, types} ->
      type =
        case types do
          [t | _] -> inspect(t)
          _ -> "any"
        end

      "    #{name} #{type}"
    end)
    |> Enum.join("\n")

    {:ok, result}
  end

  defp format_relationships(resource_name, relationships) do
    relationship_results = Enum.map(relationships, &relationship_to_mermaid(resource_name, &1))
    errors = Enum.filter(relationship_results, &match?({:error, _}, &1))

    if Enum.empty?(errors) do
      relationship_strings = Enum.map(relationship_results, fn {:ok, str} -> str end)
      {:ok, Enum.join(relationship_strings, "\n")}
    else
      {:error, errors}
    end
  end

  defp relationship_to_mermaid(source_resource, %{
         name: name,
         destination: destination,
         cardinality: cardinality,
         type: type
       }) do
    with {:ok, dest_resource} <- format_destination_resource(destination),
         {:ok, relationship_line} <- format_relationship_line(source_resource, dest_resource, name, cardinality, type) do
      {:ok, relationship_line}
    else
      {:error, error} -> {:error, error}
    end
  end

  defp format_destination_resource(destination) do
    case destination do
      mod when is_atom(mod) -> {:ok, mod |> Module.split() |> List.last()}
      _ -> {:ok, "#{destination}"}
    end
  end

  defp format_relationship_line(source_resource, dest_resource, name, cardinality, type) do
    result = case type do
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

    {:ok, result}
  end
end
