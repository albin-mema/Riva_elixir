alias RivaAsh.PropertyTesting, as: PropertyTesting
alias RivaAshWeb.Router, as: Router

defmodule RivaAsh.PropertyTesting.RouteEnumerator do
  @moduledoc """
  Automatically discovers and categorizes all application routes for property-based testing.

  This module analyzes the Phoenix router to extract all available routes and
  categorizes them by authentication requirements, user permissions, and other criteria.
  """

  @type route_category :: :public | :authenticated | :admin | :api | :error
  @type route_info :: %{
          path: String.t(),
          verb: atom(),
          controller: atom(),
          action: atom(),
          category: route_category(),
          requires_params: boolean(),
          param_types: map()
        }

  @doc """
  Get all routes from the Phoenix router and categorize them.
  """
  def enumerate_routes do
    routes =
      RivaAshWeb.Router.__routes__()
      |> Enum.map(&parse_route/1)
      |> Enum.group_by(& &1.category)

    # Log route discovery
    total_routes = routes |> Map.values() |> List.flatten() |> length()
    IO.puts("🗺️  Route Discovery: Found #{total_routes} total routes")

    Enum.each(routes, fn {category, category_routes} ->
      IO.puts("   #{category}: #{length(category_routes)} routes")
    end)

    routes
  end

  @doc """
  Get routes for a specific category.
  """
  def routes_for_category(category) do
    enumerate_routes()
    |> Map.get(category, [])
  end

  @doc """
  Get all public routes (no authentication required).
  """
  def public_routes do
    routes_for_category(:public)
  end

  @doc """
  Get all authenticated routes (login required).
  """
  def authenticated_routes do
    routes_for_category(:authenticated)
  end

  @doc """
  Get all admin routes (admin privileges required).
  """
  def admin_routes do
    routes_for_category(:admin)
  end

  @doc """
  Get routes that don't require parameters.
  """
  def parameterless_routes(category \\ :all) do
    routes =
      if category == :all do
        enumerate_routes() |> Map.values() |> List.flatten()
      else
        routes_for_category(category)
      end

    Enum.filter(routes, fn route -> not route.requires_params end)
  end

  @doc """
  Get routes that require parameters and their parameter types.
  """
  def parameterized_routes(category \\ :all) do
    routes =
      if category == :all do
        enumerate_routes() |> Map.values() |> List.flatten()
      else
        routes_for_category(category)
      end

    Enum.filter(routes, fn route -> route.requires_params end)
  end

  @doc """
  Get navigation-friendly routes (suitable for random navigation).
  """
  def navigable_routes do
    enumerate_routes()
    |> Map.take([:public, :authenticated, :admin])
    |> Map.values()
    |> List.flatten()
    |> Enum.filter(&navigable_route?/1)
  end

  @doc """
  Get CRUD operation routes grouped by resource.
  """
  def crud_routes do
    authenticated_routes()
    |> Enum.filter(&crud_route?/1)
    |> Enum.group_by(&extract_resource_name/1)
  end

  defp parse_route(route) do
    %{
      path: route.path,
      verb: route.verb,
      controller: route.plug,
      action: route.plug_opts,
      category: categorize_route(route),
      requires_params: has_parameters?(route.path),
      param_types: extract_param_types(route.path)
    }
  rescue
    error ->
      IO.puts("Error parsing route #{inspect(route)}: #{inspect(error)}")
      %{
        path: route.path,
        verb: route.verb,
        controller: route.plug,
        action: route.plug_opts,
        category: :error,
        requires_params: has_parameters?(route.path),
        param_types: %{}
      }
    end

  defp categorize_route(route) do
    cond do
      api_route?(route) -> :api
      admin_route?(route) -> :admin
      error_route?(route) -> :error
      authenticated_route?(route) -> :authenticated
      true -> :public
    end
  end

  defp api_route?(route) do
    String.starts_with?(route.path, "/api") or
      String.starts_with?(route.path, "/graphql")
  end

  defp admin_route?(route) do
    String.starts_with?(route.path, "/admin") or
      has_auth_pipeline?(route, :require_authenticated_user)
  end

  defp error_route?(route) do
    route.path in ["/404", "/access-denied", "/*path"]
  end

  defp authenticated_route?(route) do
    has_auth_pipeline?(route, :require_authenticated_user) and
      not admin_route?(route)
  end

  defp has_auth_pipeline?(route, pipeline) do
    case Map.get(route, :pipe_through) do
      pipelines when is_list(pipelines) -> pipeline in pipelines
      _unmatchedunmatched -> false
    end
  end

  defp has_parameters?(path) do
    String.contains?(path, ":")
  end

  defp extract_param_types(path) do
    Regex.scan(~r/:([^\/]+)/, path)
    |> Enum.map(fn [_full, param] -> {param, infer_param_type(param)} end)
    |> Map.new()
  end

  defp infer_param_type(param) do
    cond do
      param == "id" -> :integer
      String.ends_with?(param, "_id") -> :integer
      param in ["slug", "name"] -> :string
      true -> :string
    end
  end

  defp navigable_route?(route) do
    # Exclude routes that are not suitable for navigation testing
    excluded_patterns = [
      ~r/\/sign-out$/,
      ~r/\/auth\//,
      ~r/\.(json|xml|csv)$/,
      ~r/\/api\//
    ]

    not Enum.any?(excluded_patterns, fn pattern ->
      Regex.match?(pattern, route.path)
    end) and route.verb == :get
  end

  defp crud_route?(route) do
    crud_actions = [:index, :show, :new, :create, :edit, :update, :delete]
    route.action in crud_actions
  end

  defp extract_resource_name(route) do
    # Extract resource name from path like "/businesses" -> "business"
    case Regex.run(~r/\/([^\/]+)/, route.path) do
      [_full, resource] ->
        resource
        # Remove plural 's'
        |> String.trim_trailing("s")
        |> String.to_existing_atom()

      _unmatchedunmatched ->
        :unknown
    end
  end

  @doc """
  Generate sample parameter values for parameterized routes.
  """
  def generate_route_params(route) do
    Enum.map(route.param_types, fn {param, type} ->
      {param, generate_param_value(type, param, route)}
    end)
    |> Map.new()
  end

  defp generate_param_value(:integer, param, route) do
    # Get valid IDs from test data based on the route context
    get_valid_id_for_param(param, route)
  end

  defp generate_param_value(:string, param, _route) do
    case param do
      "slug" -> "test-slug-#{:rand.uniform(1000)}"
      "name" -> "test-name-#{:rand.uniform(1000)}"
      _unmatchedunmatched -> "test-#{param}-#{:rand.uniform(1000)}"
    end
  end

  defp generate_param_value(resource_type, param, route) when is_atom(resource_type) do
    # Handle resource types like :business, :client, etc.
    get_valid_id_for_param(param, route)
  end

  # Get valid IDs for different parameter types
  defp get_valid_id_for_param("id", route) do
    case extract_resource_from_path(route.path) do
      :business -> get_existing_business_id()
      :client -> get_existing_client_id()
      :item -> get_existing_item_id()
      :employee -> get_existing_employee_id()
      :reservation -> get_existing_reservation_id()
      :plot -> get_existing_plot_id()
      :section -> get_existing_section_id()
      :item_type -> get_existing_item_type_id()
      _ -> Enum.random(1..100) # fallback
    end
  end

  defp get_valid_id_for_param(param, _route) do
    # Handle other parameter types that might be IDs
    param_str = to_string(param)
    if String.ends_with?(param_str, "_id") do
      case param_str do
        "business_id" -> get_existing_business_id()
        "client_id" -> get_existing_client_id()
        "item_id" -> get_existing_item_id()
        "employee_id" -> get_existing_employee_id()
        "reservation_id" -> get_existing_reservation_id()
        "plot_id" -> get_existing_plot_id()
        "section_id" -> get_existing_section_id()
        "item_type_id" -> get_existing_item_type_id()
        "owner_id" -> get_existing_user_id()
        _ -> Enum.random(1..100)
      end
    else
      Enum.random(1..100)
    end
  end

  # Extract resource type from route path
  defp extract_resource_from_path(path) do
    cond do
      String.contains?(path, "/businesses") -> :business
      String.contains?(path, "/clients") -> :client
      String.contains?(path, "/items") -> :item
      String.contains?(path, "/employees") -> :employee
      String.contains?(path, "/reservations") -> :reservation
      String.contains?(path, "/plots") -> :plot
      String.contains?(path, "/sections") -> :section
      String.contains?(path, "/item-types") -> :item_type
      true -> :unknown
    end
  end

  # Functions to get existing IDs from test data
  def get_existing_business_id, do: get_random_id_from_test_data(:business)
  def get_existing_client_id, do: get_random_id_from_test_data(:client)
  def get_existing_item_id, do: get_random_id_from_test_data(:item)
  def get_existing_employee_id, do: get_random_id_from_test_data(:employee)
  def get_existing_reservation_id, do: get_random_id_from_test_data(:reservation)
  def get_existing_plot_id, do: get_random_id_from_test_data(:plot)
  def get_existing_section_id, do: get_random_id_from_test_data(:section)
  def get_existing_item_type_id, do: get_random_id_from_test_data(:item_type)
  def get_existing_user_id, do: get_random_id_from_test_data(:user)

  defp get_random_id_from_test_data(resource_type) do
    # Use the DataManager to get valid IDs
    try do
      RivaAsh.PropertyTesting.DataManager.get_random_id(resource_type)
    rescue
      _ ->
        # Fallback to reasonable ranges if DataManager fails
        case resource_type do
          :user -> Enum.random(1..5)
          :business -> Enum.random(1..10)
          :client -> Enum.random(1..20)
          :item -> Enum.random(1..15)
          :employee -> Enum.random(1..8)
          :reservation -> Enum.random(1..25)
          :plot -> Enum.random(1..5)
          :section -> Enum.random(1..10)
          :item_type -> Enum.random(1..8)
          _ -> Enum.random(1..50)
        end
    end
  end

  @doc """
  Get route patterns for StreamData generators.
  """
  def route_patterns do
    %{
      public: public_routes() |> Enum.map(& &1.path),
      authenticated: authenticated_routes() |> Enum.map(& &1.path),
      admin: admin_routes() |> Enum.map(& &1.path)
    }
  end

  @doc """
  Get weighted route selection based on typical user behavior.
  """
  def route_weights(category) do
    case category do
      :public ->
        %{
          "/" => 20,
          "/sign-in" => 30,
          "/register" => 25,
          "/404" => 5
        }

      :authenticated ->
        %{
          "/dashboard" => 25,
          "/businesses" => 20,
          "/clients" => 15,
          "/items" => 15,
          "/employees" => 10,
          "/reservations" => 10,
          "/item-holds" => 5
        }

      :admin ->
        %{
          "/admin" => 40,
          "/admin/users" => 20,
          "/admin/settings" => 20,
          "/admin/reports" => 20
        }
    end
  end

  @doc """
  Select a random route based on weights and user state.
  """
  def random_route(category, exclude_params \\ true) do
    routes =
      if exclude_params do
        parameterless_routes(category)
      else
        routes_for_category(category)
      end

    weights = route_weights(category)

    # Filter routes to only those with defined weights, or assign default weight
    weighted_routes =
      Enum.map(routes, fn route ->
        weight = Map.get(weights, route.path, 1)
        {route, weight}
      end)

    select_weighted_route(weighted_routes)
  end

  defp select_weighted_route([]) do
    # No routes available, return nil
    nil
  end

  defp select_weighted_route(weighted_routes) do
    total_weight = Enum.sum(Enum.map(weighted_routes, fn {_route, weight} -> weight end))

    # Handle case where all routes have zero weight
    if total_weight == 0 do
      # Just pick the first route if all have zero weight
      {route, _weight} = hd(weighted_routes)
      route
    else
      random_value = :rand.uniform(total_weight)
      select_route_by_weight(weighted_routes, random_value, 0)
    end
  end

  defp select_route_by_weight([{route, weight} | _rest], random_value, acc)
       when random_value <= acc + weight do
    route
  end

  defp select_route_by_weight([{_route, weight} | rest], random_value, acc) do
    select_route_by_weight(rest, random_value, acc + weight)
  end

  defp select_route_by_weight([], _random_value, _acc) do
    # Fallback - return first public route
    public_routes() |> List.first()
  end

  @doc """
  Validate that a route exists and is accessible.
  """
  def validate_route(path) do
    all_routes = enumerate_routes() |> Map.values() |> List.flatten()

    case Enum.find(all_routes, fn route -> route.path == path end) do
      nil -> {:error, :route_not_found}
      route -> {:ok, route}
    end
  end

  @doc """
  Get suggested navigation flows based on route relationships.
  """
  def suggested_flows do
    %{
      registration_flow: ["/", "/register", "/sign-in", "/dashboard"],
      business_management: ["/dashboard", "/businesses", "/businesses/new", "/businesses"],
      client_management: ["/dashboard", "/clients", "/clients/new", "/clients"],
      item_management: ["/dashboard", "/items", "/items/new", "/items"],
      admin_flow: ["/dashboard", "/admin", "/admin/users"]
    }
  end
end
