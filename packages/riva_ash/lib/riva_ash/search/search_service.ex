defmodule RivaAsh.Search.SearchService do
  @moduledoc """
  Service for handling global search operations.
  Separates business logic from LiveView concerns.
  """
  
  alias RivaAsh.Resources.{Business, Item}
  alias Ash.Query

  @doc """
  Performs global search for businesses and items based on search parameters.
  
  ## Parameters
  - search_params: Map containing search criteria (search_term, city, country)
  
  ## Returns
  {:ok, {businesses, items}} | {:error, reason}
  """
  @spec global_search(map()) :: {:ok, {[Business.t()], [Item.t()]}} | {:error, term()}
  def global_search(search_params) do
    with {:ok, businesses} <- search_businesses(search_params),
         {:ok, items} <- search_items(search_params) do
      {:ok, {businesses, items}}
    end
  end

  @doc """
  Gets available cities for search filters.
  
  ## Returns
  {:ok, [String.t()]} | {:error, reason}
  """
  @spec get_available_cities() :: {:ok, [String.t()]} | {:error, term()}
  def get_available_cities do
    case Business
         |> Query.for_read(:public_search)
         |> Query.select([:city])
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, businesses} ->
        cities = 
          businesses
          |> Enum.map(& &1.city)
          |> Enum.reject(&is_nil/1)
          |> Enum.reject(&(&1 == ""))
          |> Enum.uniq()
          |> Enum.sort()
        
        {:ok, cities}
      
      {:error, reason} -> 
        {:error, reason}
    end
  end

  @doc """
  Gets available countries for search filters.
  
  ## Returns
  {:ok, [String.t()]} | {:error, reason}
  """
  @spec get_available_countries() :: {:ok, [String.t()]} | {:error, term()}
  def get_available_countries do
    case Business
         |> Query.for_read(:public_search)
         |> Query.select([:country])
         |> Ash.read(domain: RivaAsh.Domain) do
      {:ok, businesses} ->
        countries = 
          businesses
          |> Enum.map(& &1.country)
          |> Enum.reject(&is_nil/1)
          |> Enum.reject(&(&1 == ""))
          |> Enum.uniq()
          |> Enum.sort()
        
        {:ok, countries}
      
      {:error, reason} -> 
        {:error, reason}
    end
  end

  # Private helper functions

  defp search_businesses(search_params) do
    Business
    |> Query.for_read(:public_search, search_params)
    |> Ash.read(domain: RivaAsh.Domain)
  end

  defp search_items(search_params) do
    Item
    |> Query.for_read(:public_search, search_params)
    |> Query.load([:business])
    |> Ash.read(domain: RivaAsh.Domain)
  end
end