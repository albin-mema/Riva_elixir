defmodule RivaAsh.ResourceHelpers do
  @moduledoc """
  Shared helper functions and macros for consistent resource patterns.
  Provides common attribute definitions, relationship patterns, and action templates.
  """

  @doc """
  Standard attributes that most resources should have.
  """
  defmacro standard_attributes do
    quote do
      uuid_primary_key(:id)

      create_timestamp(:inserted_at)
      update_timestamp(:updated_at)
    end
  end

  @doc """
  Standard name attribute with consistent constraints.
  """
  defmacro name_attribute(opts \\ []) do
    min_length = Keyword.get(opts, :min_length, 2)
    max_length = Keyword.get(opts, :max_length, 100)
    description = Keyword.get(opts, :description, "Name of the resource")

    quote do
      attribute :name, :string do
        allow_nil?(false)
        public?(true)
        constraints [
          min_length: unquote(min_length),
          max_length: unquote(max_length),
          trim?: true,
          allow_empty?: false
        ]
        description(unquote(description))
      end
    end
  end

  @doc """
  Standard active flag attribute.
  """
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
  defmacro description_attribute(opts \\ []) do
    max_length = Keyword.get(opts, :max_length, 500)
    required = Keyword.get(opts, :required, false)

    quote do
      attribute :description, :string do
        allow_nil?(not unquote(required))
        public?(true)
        constraints [
          max_length: unquote(max_length),
          trim?: true
        ]
        description("Description of the resource")
      end
    end
  end

  @doc """
  Standard admin configuration.
  """
  defmacro standard_admin(table_columns) do
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
  defmacro standard_paper_trail(opts \\ []) do
    quote do
      paper_trail do
        # Track all changes with full diffs
        change_tracking_mode(:full_diff)

        # Don't store timestamps in the changes by default
        ignore_attributes(unquote(Keyword.get(opts, :ignore_attributes, [:inserted_at, :updated_at])))

        # Store action name for better auditing
        store_action_name?(true)

        # Store action inputs for better auditing
        store_action_inputs?(true)

        # Store resource identifier for better querying
        store_resource_identifier?(true)

        # Create versions on destroy (for soft deletes) - can be overridden
        create_version_on_destroy?(unquote(Keyword.get(opts, :create_version_on_destroy?, true)))
      end
    end
  end

  @doc """
  Standard postgres configuration.
  """
  defmacro standard_postgres(table_name) do
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
  defmacro standard_business_resource(table_name, admin_columns \\ []) do
    quote do
      standard_postgres(unquote(table_name))
      standard_archive()
      standard_paper_trail()

      unless unquote(admin_columns) == [] do
        standard_admin(unquote(admin_columns))
      end
    end
  end

  @doc """
  Standard configuration for join table resources (like EmployeePermission).
  Includes postgres, archival, and paper trail but no admin interface.
  """
  defmacro standard_join_resource(table_name) do
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
  defmacro standard_lookup_resource(table_name, admin_columns) do
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
  defmacro standard_json_api(type_name, base_route \\ nil) do
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
  defmacro standard_timestamps do
    quote do
      create_timestamp(:inserted_at)
      update_timestamp(:updated_at)
    end
  end

  @doc """
  Standard UUID primary key.
  """
  defmacro standard_uuid_primary_key do
    quote do
      uuid_primary_key(:id)
    end
  end

  @doc """
  Standard read actions that most resources need.
  """
  defmacro standard_read_actions do
    quote do
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
  defmacro business_scoped_actions do
    quote do
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
  defmacro standard_json_api_routes(type_name) do
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
  defmacro standard_graphql(type_name) do
    quote do
      graphql do
        type(unquote(type_name))

        queries do
          get(:"get_#{unquote(type_name)}", :read)
          list(:"list_#{unquote(type_name)}s", :read)
          list(:"active_#{unquote(type_name)}s", :active)
          list(:"inactive_#{unquote(type_name)}s", :inactive)
        end

        mutations do
          create(:"create_#{unquote(type_name)}", :create)
          update(:"update_#{unquote(type_name)}", :update)
          destroy(:"delete_#{unquote(type_name)}", :destroy)
        end
      end
    end
  end

  @doc """
  Standard code interface.
  """
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
  def choices_for_select(resource_module) do
    resource_module
    |> Ash.read!()
    |> Enum.map(fn record ->
      {record.id, record.name}
    end)
  end
end
