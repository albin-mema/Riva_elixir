defmodule RivaAsh.ResourceHelpers do
  @moduledoc """
  Shared helper functions and macros for consistent resource patterns.
  Provides common attribute definitions, relationship patterns, and action templates.
  """

  import Ash.Expr

  @doc """
  Standard attributes that most resources should have.
  """
  @spec standard_attributes() :: Macro.t()
  defmacro standard_attributes do
    quote do
      uuid_primary_key(:id)
      standard_timestamps()
    end
  end

  @doc """
  Standard name attribute with consistent constraints.
  """
  @spec name_attribute(Keyword.t()) :: Macro.t()
  defmacro name_attribute(opts \\ []) do
    with {:ok, min_length} <- validate_min_length(Keyword.get(opts, :min_length, 2)),
         {:ok, max_length} <- validate_max_length(Keyword.get(opts, :max_length, 100)),
         {:ok, description} <- validate_description(Keyword.get(opts, :description, "Name of the resource")) do
      build_name_attribute(min_length, max_length, description)
    else
      {:error, reason} -> raise ArgumentError, "Invalid name_attribute options: #{reason}"
    end
  end

  @spec validate_min_length(integer()) :: {:ok, integer()} | {:error, String.t()}
  defp validate_min_length(length) when is_integer(length) and length >= 0 do
    {:ok, length}
  end

  defp validate_min_length(_), do: {:error, "min_length must be a non-negative integer"}

  @spec validate_max_length(integer()) :: {:ok, integer()} | {:error, String.t()}
  defp validate_max_length(length) when is_integer(length) and length >= 0 do
    {:ok, length}
  end

  defp validate_max_length(_), do: {:error, "max_length must be a non-negative integer"}

  @spec validate_description(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  defp validate_description(description) when is_binary(description) do
    {:ok, description}
  end

  defp validate_description(_), do: {:error, "description must be a string"}

  @spec build_name_attribute(integer(), integer(), String.t()) :: Macro.t()
  defp build_name_attribute(min_length, max_length, description) do
    quote do
      attribute :name, :string do
        allow_nil?(false)
        public?(true)

        constraints(
          min_length: unquote(min_length),
          max_length: unquote(max_length),
          trim?: true,
          allow_empty?: false
        )

        description(unquote(description))
      end
    end
  end

  @doc """
  Standard active flag attribute.
  """
  @spec active_attribute() :: Macro.t()
  defmacro active_attribute do
    quote do
      attribute :is_active, :boolean do
        default(true)
        public?(true)
        description("Whether this record is active")
      end
    end
  end

  @doc """
  Standard business relationship for business-scoped resources.
  """
  @spec business_relationship() :: Macro.t()
  defmacro business_relationship do
    quote do
      belongs_to :business, RivaAsh.Resources.Business do
        allow_nil?(false)
        attribute_writable?(true)
        public?(true)
        description("The business this resource belongs to")
      end
    end
  end

  @doc """
  Standard description attribute.
  """
  @spec description_attribute(Keyword.t()) :: Macro.t()
  defmacro description_attribute(opts \\ []) do
    with {:ok, max_length} <- validate_max_length(Keyword.get(opts, :max_length, 500)),
         {:ok, required} <- validate_required(Keyword.get(opts, :required, false)) do
      build_description_attribute(max_length, required)
    else
      {:error, reason} -> raise ArgumentError, "Invalid description_attribute options: #{reason}"
    end
  end

  @spec validate_required(boolean()) :: {:ok, boolean()} | {:error, String.t()}
  defp validate_required(required) when is_boolean(required) do
    {:ok, required}
  end

  defp validate_required(_), do: {:error, "required must be a boolean"}

  @spec build_description_attribute(integer(), boolean()) :: Macro.t()
  defp build_description_attribute(max_length, required) do
    quote do
      attribute :description, :string do
        allow_nil?(not unquote(required))
        public?(true)

        constraints(
          max_length: unquote(max_length),
          trim?: true
        )

        description("Description of the resource")
      end
    end
  end

  @doc """
  Standard admin configuration.
  """
  @spec standard_admin(list()) :: Macro.t()
  defmacro standard_admin(table_columns) when is_list(table_columns) do
    quote do
      admin do
        table_columns(unquote(table_columns))
        relationship_display_fields([:name])
      end
    end
  end

  @doc """
  Standard archival configuration.
  """
  @spec standard_archive() :: Macro.t()
  defmacro standard_archive do
    quote do
      archive do
        attribute(:archived_at)
        base_filter?(false)
      end
    end
  end

  @doc """
  Standard paper trail configuration for audit tracking.
  """
  @spec standard_paper_trail(Keyword.t()) :: Macro.t()
  defmacro standard_paper_trail(opts \\ []) do
    with {:ok, ignore_attributes} <-
           validate_ignore_attributes(Keyword.get(opts, :ignore_attributes, [:inserted_at, :updated_at])),
         {:ok, create_version_on_destroy} <-
           validate_create_version_on_destroy(Keyword.get(opts, :create_version_on_destroy?, true)) do
      build_paper_trail_config(ignore_attributes, create_version_on_destroy)
    else
      {:error, reason} -> raise ArgumentError, "Invalid standard_paper_trail options: #{reason}"
    end
  end

  @spec validate_ignore_attributes(list()) :: {:ok, list()} | {:error, String.t()}
  defp validate_ignore_attributes(attributes) when is_list(attributes) do
    if Enum.all?(attributes, &is_atom/1) do
      {:ok, attributes}
    else
      {:error, "ignore_attributes must be a list of atoms"}
    end
  end

  defp validate_ignore_attributes(_), do: {:error, "ignore_attributes must be a list"}

  @spec validate_create_version_on_destroy(boolean()) :: {:ok, boolean()} | {:error, String.t()}
  defp validate_create_version_on_destroy(value) when is_boolean(value) do
    {:ok, value}
  end

  defp validate_create_version_on_destroy(_), do: {:error, "create_version_on_destroy? must be a boolean"}

  @spec build_paper_trail_config(list(), boolean()) :: Macro.t()
  defp build_paper_trail_config(ignore_attributes, create_version_on_destroy) do
    quote do
      paper_trail do
        change_tracking_mode(:full_diff)
        ignore_attributes(unquote(ignore_attributes))
        store_action_name?(true)
        store_action_inputs?(true)
        store_resource_identifier?(true)
        create_version_on_destroy?(unquote(create_version_on_destroy))
      end
    end
  end

  @doc """
  Standard postgres configuration.
  """
  @spec standard_postgres(String.t()) :: Macro.t()
  defmacro standard_postgres(table_name) when is_binary(table_name) do
    quote do
      postgres do
        table(unquote(table_name))
        repo(RivaAsh.Repo)
      end
    end
  end

  @doc """
  Standard extensions for most resources.
  """
  @spec standard_extensions() :: Macro.t()
  defmacro standard_extensions do
    quote do
      use Ash.Resource,
        extensions: [
          AshJsonApi.Resource,
          AshGraphql.Resource,
          AshPaperTrail.Resource,
          AshArchival.Resource,
          AshAdmin.Resource
        ]
    end
  end

  @doc """
  Complete standard resource configuration for business domain resources.
  Includes postgres, archival, and paper trail configurations.
  """
  @spec standard_business_resource(String.t(), list()) :: Macro.t()
  defmacro standard_business_resource(table_name, admin_columns \\ [])
           when is_binary(table_name) and is_list(admin_columns) do
    quote do
      standard_postgres(unquote(table_name))
      standard_archive()
      standard_paper_trail()

      if unquote(admin_columns) != [] do
        standard_admin(unquote(admin_columns))
      end
    end
  end

  @doc """
  Standard configuration for join table resources (like EmployeePermission).
  Includes postgres, archival, and paper trail but no admin interface.
  """
  @spec standard_join_resource(String.t()) :: Macro.t()
  defmacro standard_join_resource(table_name) when is_binary(table_name) do
    quote do
      standard_postgres(unquote(table_name))
      standard_archive()
      standard_paper_trail()
    end
  end

  @doc """
  Standard configuration for lookup/reference resources (like Permission).
  Includes postgres, archival, paper trail, and admin interface.
  """
  @spec standard_lookup_resource(String.t(), list()) :: Macro.t()
  defmacro standard_lookup_resource(table_name, admin_columns)
           when is_binary(table_name) and is_list(admin_columns) do
    quote do
      standard_postgres(unquote(table_name))
      standard_archive()
      standard_paper_trail()
      standard_admin(unquote(admin_columns))
    end
  end

  @doc """
  Standard JSON API configuration for most resources.
  """
  @spec standard_json_api(String.t(), String.t() | nil) :: Macro.t()
  defmacro standard_json_api(type_name, base_route \\ nil)
           when is_binary(type_name) do
    base_route = base_route || "/#{String.replace(type_name, "_", "-")}s"

    quote do
      json_api do
        type(unquote(type_name))

        routes do
          base(unquote(base_route))
          get(:read)
          index(:read)
          post(:create)
          patch(:update)
          delete(:destroy)
        end
      end
    end
  end

  @doc """
  Standard GraphQL configuration for most resources.
  """
  @spec standard_graphql() :: Macro.t()
  defmacro standard_graphql do
    quote do
      graphql do
        type(:read)
        type(:create)
        type(:update)
        type(:destroy)
      end
    end
  end

  @doc """
  Standard attributes that most business resources need.
  """
  @spec standard_timestamps() :: Macro.t()
  defmacro standard_timestamps do
    quote do
      create_timestamp(:inserted_at)
      update_timestamp(:updated_at)
    end
  end

  @doc """
  Standard UUID primary key.
  """
  @spec standard_uuid_primary_key() :: Macro.t()
  defmacro standard_uuid_primary_key do
    quote do
      uuid_primary_key(:id)
    end
  end

  @doc """
  Standard read actions that most resources need.
  """
  @spec standard_read_actions() :: Macro.t()
  defmacro standard_read_actions do
    quote do
      import Ash.Expr

      read :by_id do
        argument(:id, :uuid, allow_nil?: false)
        get?(true)
        filter(expr(id == ^arg(:id)))
      end

      read :active do
        filter(expr(is_active == true and is_nil(archived_at)))
      end

      read :inactive do
        filter(expr(is_active == false or not is_nil(archived_at)))
      end
    end
  end

  @doc """
  Standard business-scoped read actions.
  """
  @spec business_scoped_actions() :: Macro.t()
  defmacro business_scoped_actions do
    quote do
      import Ash.Expr

      read :by_business do
        argument(:business_id, :uuid, allow_nil?: false)
        filter(expr(business_id == ^arg(:business_id)))
      end

      read :by_business_active do
        argument(:business_id, :uuid, allow_nil?: false)
        filter(expr(business_id == ^arg(:business_id) and is_active == true and is_nil(archived_at)))
      end
    end
  end

  @doc """
  Standard JSON API routes.
  """
  @spec standard_json_api_routes(String.t()) :: Macro.t()
  defmacro standard_json_api_routes(type_name) when is_binary(type_name) do
    quote do
      json_api do
        type(unquote(type_name))

        routes do
          base("/#{unquote(type_name)}s")
          get(:read)
          index(:read)
          post(:create)
          patch(:update)
          delete(:destroy)
          get(:active, route: "/active")
          get(:inactive, route: "/inactive")
        end
      end
    end
  end

  @doc """
  Standard GraphQL configuration.
  """
  @spec standard_graphql(String.t() | atom()) :: Macro.t()
  defmacro standard_graphql(type_name) when is_binary(type_name) do
    build_graphql_config(type_name, type_name)
  end

  defmacro standard_graphql(type_name) when is_atom(type_name) do
    type_name_str = Atom.to_string(type_name)
    build_graphql_config(type_name, type_name_str)
  end

  @spec build_graphql_config(String.t(), String.t()) :: Macro.t()
  defp build_graphql_config(type_name, type_name_str) do
    get_atom = String.to_existing_atom("get_#{type_name}")
    list_atom = String.to_existing_atom("list_#{type_name_str}s")
    active_atom = String.to_existing_atom("active_#{type_name_str}s")
    inactive_atom = String.to_existing_atom("inactive_#{type_name_str}s")
    create_atom = String.to_existing_atom("create_#{type_name}")
    update_atom = String.to_existing_atom("update_#{type_name}")
    delete_atom = String.to_existing_atom("delete_#{type_name}")

    quote do
      graphql do
        type(unquote(type_name))

        queries do
          get(unquote(get_atom), :read)
          list(unquote(list_atom), :read)
          list(unquote(active_atom), :active)
          list(unquote(inactive_atom), :inactive)
        end

        mutations do
          create(unquote(create_atom), :create)
          update(unquote(update_atom), :update)
          destroy(unquote(delete_atom), :destroy)
        end
      end
    end
  end

  @doc """
  Standard code interface.
  """
  @spec standard_code_interface() :: Macro.t()
  defmacro standard_code_interface do
    quote do
      code_interface do
        define(:create, action: :create)
        define(:read, action: :read)
        define(:update, action: :update)
        define(:destroy, action: :destroy)
        define(:by_id, args: [:id], action: :by_id)
        define(:active, action: :active)
        define(:inactive, action: :inactive)
      end
    end
  end

  @doc """
  Standard validations that most resources should have.
  """
  @spec standard_validations() :: Macro.t()
  defmacro standard_validations do
    quote do
      validations do
        validate(present([:name]), message: "Name is required")
        validate({RivaAsh.Validations, :sanitize_text_input}, field: :name)
      end
    end
  end

  @doc """
  Helper function for admin dropdowns.
  """
  @spec choices_for_select(atom()) :: list({String.t(), String.t()})
  def choices_for_select(resource_module) when is_atom(resource_module) do
    case Ash.read(resource_module) do
      {:ok, records} ->
        Enum.map(records, fn record ->
          {record.id, record.name}
        end)

      {:error, reason} ->
        raise "Failed to read records from #{resource_module}: #{inspect(reason)}"
    end
  end

  @doc """
  Standard authorization policies for business-scoped resources.
  Ensures users can only access resources belonging to their business.
  """
  @spec business_scoped_policies() :: Macro.t()
  defmacro business_scoped_policies do
    quote location: :keep do
      import Ash.Expr

      # Policy for business-scoped access
      policy action_type([:read, :create, :update, :destroy]) do
        authorize_if(expr(^actor(:current_business_id) == record.business_id))
      end

      # Policy for admin access (bypasses business scope)
      policy action_type([:read, :create, :update, :destroy]) do
        authorize_if(expr(actor(:role) in [:admin, :super_admin]))
      end
    end
  end
end
