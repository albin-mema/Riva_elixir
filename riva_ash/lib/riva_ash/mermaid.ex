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
    resources = Ash.Domain.Info.resources(domain)

    """
    erDiagram
    #{Enum.map_join(resources, "\n\n", &resource_to_mermaid/1)}
    """
    |> String.trim()
  end

  defp resource_to_mermaid(resource) do
    attributes = resource.__info__(:attributes)
    relationships = Ash.Resource.Info.relationships(resource)
    resource_name = resource |> Module.split() |> List.last()

    attributes_str =
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

    relationships_str =
      relationships
      |> Enum.map_join("\n", &relationship_to_mermaid(resource_name, &1))

    """
    #{resource_name} {
      #{attributes_str}
      #{relationships_str}
    }
    """
    |> String.trim()
  end

  defp relationship_to_mermaid(source_resource, %{
    name: name,
    destination: destination,
    cardinality: cardinality,
    type: type
  }) do
    dest_resource = 
      case destination do
        mod when is_atom(mod) -> mod |> Module.split() |> List.last()
        _ -> "#{destination}"
      end
    
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
