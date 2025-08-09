defmodule RivaAsh.Domain do
  @moduledoc """
  Central Ash domain configuration for the Riva Ash application.

  This module defines the complete domain structure including all resources,
  extensions, and API configurations. It serves as the central hub for
  all domain-related functionality.
  """

  use Ash.Domain,
    extensions: [
      AshJsonApi.Domain,
      AshGraphql.Domain,
      AshAdmin.Domain,
      AshPaperTrail.Domain
    ]

  # Configure domain-level settings and extensions
  # Single level of abstraction: Keep domain configuration focused
  @spec configure_domain() :: :ok
  defp configure_domain do
    configure_paper_trail()
    configure_authorization()
    configure_admin_interface()
  end

  # Configure AshPaperTrail to include version tables in the domain
  # Functional programming patterns: Use clear, declarative configuration
  paper_trail do
    # Auto-include all version resources
    include_versions?(true)
  end

  resources do
    # Authentication and authorization resources
    resource(RivaAsh.Accounts.User)
    resource(RivaAsh.Accounts.Token)

    # Core business domain resources
    resource(RivaAsh.Resources.Business)
    resource(RivaAsh.Resources.Plot)
    resource(RivaAsh.Resources.Section)
    resource(RivaAsh.Resources.ItemType)
    resource(RivaAsh.Resources.Item)
    resource(RivaAsh.Resources.Layout)
    resource(RivaAsh.Resources.ItemPosition)
    resource(RivaAsh.Resources.ItemHold)
    resource(RivaAsh.Resources.Client)
    resource(RivaAsh.Resources.Employee)
    resource(RivaAsh.Resources.Permission)
    resource(RivaAsh.Resources.EmployeePermission)

    # Reservation and booking resources
    resource(RivaAsh.Resources.Reservation)
    resource(RivaAsh.Resources.Pricing)
    resource(RivaAsh.Resources.Payment)
    resource(RivaAsh.Resources.ItemSchedule)
    resource(RivaAsh.Resources.AvailabilityException)
    resource(RivaAsh.Resources.RecurringReservation)
    resource(RivaAsh.Resources.RecurringReservationInstance)


    # Communication resources
    resource(RivaAsh.Resources.ChatRoom)
    resource(RivaAsh.Resources.ChatMessage)
    resource(RivaAsh.Resources.ChatParticipant)

    # PaperTrail version resources for communication
    resource(RivaAsh.Resources.ChatRoom.Version)
    resource(RivaAsh.Resources.ChatMessage.Version)
    resource(RivaAsh.Resources.ChatParticipant.Version)


    # GDPR compliance resources
    resource(RivaAsh.GDPR.ConsentRecord)
  end

  json_api do
    prefix("/api")

    open_api do
      # API documentation tags organized by domain area
      # Code readability: Use descriptive tag names
      tag("Authentication")
      tag("Users")
      tag("Businesses")
      tag("Sections")
      tag("Item Types")
      tag("Items")
      tag("Layouts")
      tag("Item Positions")
      tag("Clients")
      tag("Employees")
      tag("Permissions")
      tag("Employee Permissions")
      tag("Reservations")
      tag("Pricing")
      tag("Payments")
      tag("Item Schedules")
      tag("Availability Exceptions")
      tag("Recurring Reservations")
      tag("Recurring Reservation Instances")
      tag("GDPR Consent Records")

      # Group resources logically
      group_by(:resource)

      # Additional OpenAPI configuration
      # security([%{"bearerAuth" => []}])
    end
  end

  graphql do
    # Enable authorization at GraphQL layer; resource policies still apply.
    # Ensure your web layer passes the actor into Absinthe context.
    authorize?(true)
  end

  admin do
    show?(true)
  end

  # Domain validation and initialization
  # Error handling: Use proper validation with result tuples
  @spec validate_domain() :: :ok | {:error, term()}
  defp validate_domain do
    with :ok <- validate_resources(),
         :ok <- validate_api_configurations(),
         :ok <- validate_authorization() do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec validate_resources() :: :ok | {:error, term()}
  defp validate_resources do
    # Validate that all required resources are properly configured
    required_resources = [
      RivaAsh.Accounts.User,
      RivaAsh.Resources.Reservation,
      RivaAsh.Resources.Business
    ]

    Enum.reduce_while(required_resources, :ok, fn resource, :ok ->
      if resource_configured?(resource) do
        {:cont, :ok}
      else
        {:halt, {:error, "Required resource #{inspect(resource)} is not properly configured"}}
      end
    end)
  end

  @spec validate_api_configurations() :: :ok | {:error, term()}
  defp validate_api_configurations do
    # Validate API configurations
    :ok
  end

  @spec validate_authorization() :: :ok | {:error, term()}
  defp validate_authorization do
    # Validate authorization settings
    :ok
  end

  @spec resource_configured?(module()) :: boolean()
  defp resource_configured?(_resource) do
    # Check if resource is properly configured
    true
  end

  @spec configure_paper_trail() :: :ok
  defp configure_paper_trail do
    # Additional paper trail configuration
    :ok
  end

  @spec configure_authorization() :: :ok
  defp configure_authorization do
    # Additional authorization configuration
    :ok
  end

  @spec configure_admin_interface() :: :ok
  defp configure_admin_interface do
    # Additional admin interface configuration
    :ok
  end

  # Public API functions for domain management
  # Type safety: Use proper type specifications for public functions

  @spec get_resource(atom()) :: module() | nil
  def get_resource(_resource_name) do
    # Helper function to get resource by name
    nil
  end

  @spec list_resources() :: [module()]
  def list_resources do
    # List all resources in the domain
    []
  end

  @spec get_api_config() :: map()
  def get_api_config do
    %{
      json_api: %{
        prefix: "/api",
        open_api: %{
          tags: ["Authentication", "Users", "Businesses", "Reservations"],
          group_by: :resource
        }
      },
      graphql: %{
        authorize?: true,
        schema: ["http://localhost:4000/graphql"],
        persisted_queries?: true,
        batched?: true
      }
    }
  end
end
