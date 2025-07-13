defmodule RivaAsh.Mermaid do
  @moduledoc """
  Generates Mermaid.js diagrams from Ash resources.
  """
  import OK, only: [success: 1, failure: 1, ~>>: 2, for: 1, map_all: 2]

  @doc """
  Generates a Mermaid ERD diagram from Ash resources.

  ## Examples

      iex> RivaAsh.Mermaid.generate_erd(RivaAsh.Domain)
  """
  def generate_erd(domain) do
    OK.for do
      resources <- OK.wrap(Ash.Domain.Info.resources(domain))
      mermaid_resources <- OK.map_all(resources, &resource_to_mermaid/1)
    after
      """
      erDiagram
      #{Enum.join(mermaid_resources, "\n\n")}
      """
      |> String.trim()
    end
  end

  defp resource_to_mermaid(resource) do
    OK.for do
      attributes <- OK.wrap(resource.__info__(:attributes))
      relationships <- OK.wrap(Ash.Resource.Info.relationships(resource))
      resource_name <- resource |> Module.split() |> List.last() |> success()
      attributes_str <- format_attributes(attributes)
      relationships_str <- format_relationships(resource_name, relationships)
    after
      """
      #{resource_name} {
        #{attributes_str}
        #{relationships_str}
      }
      """
      |> String.trim()
    end
  end

  defp format_attributes(attributes) do
    attributes
    |> Enum.map(fn {name, types} ->
      type =
        case types do
          [t | _] -> inspect(t)
          _ -> "any"
        end

      "    #{name} #{type}"
    end)
    |> Enum.join("\n")
    |> success()
  end

  defp format_relationships(resource_name, relationships) do
    relationships
    |> OK.map_all(&relationship_to_mermaid(resource_name, &1))
    ~>> fn relationship_strings ->
      Enum.join(relationship_strings, "\n")
    end
  end

  defp relationship_to_mermaid(source_resource, %{
         name: name,
         destination: destination,
         cardinality: cardinality,
         type: type
       }) do
    OK.for do
      dest_resource <- format_destination_resource(destination)
      relationship_line <- format_relationship_line(source_resource, dest_resource, name, cardinality, type)
    after
      relationship_line
    end
  end

  defp format_destination_resource(destination) do
    case destination do
      mod when is_atom(mod) -> mod |> Module.split() |> List.last() |> success()
      _ -> "#{destination}" |> success()
    end
  end

  defp format_relationship_line(source_resource, dest_resource, name, cardinality, type) do
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
    |> success()
  end
end
