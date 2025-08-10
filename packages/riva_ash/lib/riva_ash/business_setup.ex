defmodule RivaAsh.BusinessSetup do
  @moduledoc """
  Business setup and configuration module.
  
  This module provides functionality for:
  - Business onboarding
  - Business configuration
  - Initial setup workflows
  - Template management
  - Setup validation
  """

  require Logger

  @type business :: map()
  @type setup_params :: map()
  @type setup_result :: {:ok, business()} | {:error, String.t()}
  @type template :: map()

  @doc """
  Creates a new business with initial setup.
  
  ## Parameters
    - setup_params: Parameters for business setup
    - owner_id: ID of the business owner
  
  ## Returns
    - {:ok, business} - Successfully created business
    - {:error, reason} - Setup failed
  """
  @spec setup_business(setup_params(), String.t()) :: setup_result()
  def setup_business(setup_params, owner_id) when is_binary(owner_id) do
    Logger.info("Setting up business for owner: #{owner_id}")
    
    with {:ok, validated_params} <- validate_setup_params(setup_params),
         :ok <- validate_business_requirements(validated_params),
         business_data <- prepare_business_data(validated_params, owner_id),
         {:ok, business} <- create_business(business_data),
         :ok <- setup_initial_configuration(business),
         :ok <- setup_default_templates(business),
         :ok <- setup_initial_users(business, owner_id) do
      {:ok, business}
    else
      {:error, reason} -> {:error, format_error(reason)}
    end
  end

  @doc """
  Gets business setup templates.
  
  ## Parameters
    - business_type: Type of business (optional)
  
  ## Returns
    - {:ok, templates} - List of available templates
    - {:error, reason} - Failed to get templates
  """
  @spec get_setup_templates(String.t() | nil) :: {:ok, [template()]} | {:error, String.t()}
  def get_setup_templates(business_type \\ nil) do
    Logger.debug("Getting setup templates for business type: #{business_type}")
    
    try do
      templates = case business_type do
        nil -> get_all_templates()
        type -> get_templates_by_type(type)
      end
      
      {:ok, templates}
    rescue
      error ->
        Logger.error("Error getting setup templates: #{inspect(error)}")
        {:error, "Failed to retrieve setup templates"}
    end
  end

  @doc """
  Applies a template to a business.
  
  ## Parameters
    - business_id: ID of the business
    - template_id: ID of the template to apply
    - actor: The user applying the template
  
  ## Returns
    - {:ok, business} - Successfully updated business
    - {:error, reason} - Template application failed
  """
  @spec apply_template(String.t(), String.t(), map()) :: setup_result()
  def apply_template(business_id, template_id, actor) when is_binary(business_id) and is_binary(template_id) do
    Logger.info("Applying template #{template_id} to business #{business_id}")
    
    with {:ok, business} <- get_business(business_id),
         :ok <- validate_business_ownership(business, actor),
         {:ok, template} <- get_template(template_id),
         :ok <- validate_template_compatibility(template, business),
         :ok <- apply_template_configuration(business, template),
         :ok <- apply_template_services(business, template),
         :ok <- apply_template_pricing(business, template) do
      {:ok, business}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Validates business setup parameters.
  
  ## Parameters
    - setup_params: Parameters to validate
  
  ## Returns
    - {:ok, validated_params} - Validated parameters
    - {:error, reason} - Validation failed
  """
  @spec validate_setup_params(setup_params()) :: {:ok, setup_params()} | {:error, String.t()}
  def validate_setup_params(setup_params) do
    Logger.debug("Validating setup parameters")
    
    required_fields = [:name, :business_type, :owner_id]
    
    case validate_required_fields(setup_params, required_fields) do
      :ok ->
        with :ok <- validate_business_name(setup_params),
             :ok <- validate_business_type(setup_params),
             :ok <- validate_business_category(setup_params),
             :ok <- validate_contact_info(setup_params) do
          {:ok, setup_params}
        else
          {:error, reason} -> {:error, reason}
        end
      
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Gets business setup progress.
  
  ## Parameters
    - business_id: ID of the business
  
  ## Returns
    - {:ok, progress} - Setup progress information
    - {:error, reason} - Failed to get progress
  """
  @spec get_setup_progress(String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_setup_progress(business_id) when is_binary(business_id) do
    Logger.debug("Getting setup progress for business: #{business_id}")
    
    try do
      progress = %{
        business_id: business_id,
        completed_steps: get_completed_steps(business_id),
        pending_steps: get_pending_steps(business_id),
        overall_progress: calculate_overall_progress(business_id),
        next_steps: get_next_steps(business_id),
        estimated_completion_time: get_estimated_completion_time(business_id),
        last_updated: DateTime.utc_now()
      }
      
      {:ok, progress}
    rescue
      error ->
        Logger.error("Error getting setup progress: #{inspect(error)}")
        {:error, "Failed to retrieve setup progress"}
    end
  end

  @doc """
  Completes a setup step.
  
  ## Parameters
    - business_id: ID of the business
    - step_name: Name of the step to complete
    - step_data: Data for the step (optional)
    - actor: The user completing the step
  
  ## Returns
    - {:ok, progress} - Updated progress information
    - {:error, reason} - Step completion failed
  """
  @spec complete_setup_step(String.t(), String.t(), map() | nil, map()) :: {:ok, map()} | {:error, String.t()}
  def complete_setup_step(business_id, step_name, step_data \\ nil, actor) when is_binary(business_id) do
    Logger.info("Completing setup step #{step_name} for business #{business_id}")
    
    with {:ok, business} <- get_business(business_id),
         :ok <- validate_business_ownership(business, actor),
         :ok <- validate_step_eligibility(step_name, business),
         :ok <- process_step_data(step_name, step_data, business),
         :ok <- mark_step_completed(business_id, step_name) do
      get_setup_progress(business_id)
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Gets business configuration checklist.
  
  ## Parameters
    - business_type: Type of business
  
  ## Returns
    - {:ok, checklist} - Configuration checklist
    - {:error, reason} - Failed to get checklist
  """
  @spec get_configuration_checklist(String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_configuration_checklist(business_type) when is_binary(business_type) do
    Logger.debug("Getting configuration checklist for business type: #{business_type}")
    
    try do
      checklist = %{
        business_type: business_type,
        required_items: get_required_items(business_type),
        optional_items: get_optional_items(business_type),
        recommendations: get_recommendations(business_type),
        dependencies: get_step_dependencies(business_type),
        estimated_time: get_estimated_setup_time(business_type),
        generated_at: DateTime.utc_now()
      }
      
      {:ok, checklist}
    rescue
      error ->
        Logger.error("Error getting configuration checklist: #{inspect(error)}")
        {:error, "Failed to retrieve configuration checklist"}
    end
  end

  # Private helper functions

  defp validate_required_fields(params, required_fields) do
    missing_fields = Enum.filter(required_fields, &(!Map.has_key?(params, &1)))
    
    case length(missing_fields) do
      0 -> :ok
      _ -> {:error, "Missing required fields: #{inspect(missing_fields)}"}
    end
  end

  defp validate_business_name(params) do
    case Map.get(params, :name) do
      name when is_binary(name) and byte_size(name) > 2 -> :ok
      _ -> {:error, "Business name must be at least 3 characters long"}
    end
  end

  defp validate_business_type(params) do
    case Map.get(params, :business_type) do
      type when type in ["restaurant", "hotel", "spa", "fitness", "retail", "service", "other"] -> :ok
      _ -> {:error, "Invalid business type"}
    end
  end

  defp validate_business_category(params) do
    case Map.get(params, :category) do
      category when is_binary(category) and byte_size(category) > 0 -> :ok
      nil -> :ok
      _ -> {:error, "Invalid business category"}
    end
  end

  defp validate_contact_info(params) do
    case {Map.get(params, :email), Map.get(params, :phone)} do
      {email, phone} when is_binary(email) and is_binary(phone) and byte_size(phone) >= 10 ->
        :ok
      {email, nil} when is_binary(email) ->
        if String.match?(email, ~r/^[^\s@]+@[^\s@]+\.[^\s@]+$/) do
          :ok
        else
          {:error, "Invalid email format"}
        end
      {nil, phone} when is_binary(phone) and byte_size(phone) >= 10 ->
        :ok
      _ -> {:error, "Valid email or phone number is required"}
    end
  end

  defp validate_business_requirements(_params) do
    # This would typically validate business-specific requirements
    # For now, return :ok as stub implementation
    :ok
  end

  defp prepare_business_data(params, owner_id) do
    %{
      name: Map.get(params, :name),
      description: Map.get(params, :description, ""),
      business_type: Map.get(params, :business_type),
      category: Map.get(params, :category),
      email: Map.get(params, :email),
      phone: Map.get(params, :phone),
      address: Map.get(params, :address, %{}),
      owner_id: owner_id,
      status: "active",
      settings: Map.get(params, :settings, %{}),
      setup_completed: false,
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }
  end

  defp create_business(business_data) do
    # This would typically create the business in the database
    # For now, return the business data as stub implementation
    {:ok, business_data}
  end

  defp setup_initial_configuration(_business) do
    # This would typically set up initial business configuration
    # For now, return :ok as stub implementation
    :ok
  end

  defp setup_default_templates(_business) do
    # This would typically set up default templates
    # For now, return :ok as stub implementation
    :ok
  end

  defp setup_initial_users(_business, _owner_id) do
    # This would typically set up initial users
    # For now, return :ok as stub implementation
    :ok
  end

  defp get_business(business_id) do
    # This would typically fetch the business from the database
    # For now, return empty business as stub implementation
    {:ok, %{
      id: business_id,
      name: "Sample Business",
      business_type: "restaurant",
      owner_id: "owner-123",
      status: "active",
      setup_completed: false,
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }}
  end

  defp validate_business_ownership(business, actor) do
    if business.owner_id == actor.id do
      :ok
    else
      {:error, "You don't have permission to modify this business"}
    end
  end

  defp get_template(template_id) do
    # This would typically fetch the template from the database
    # For now, return empty template as stub implementation
    {:ok, %{
      id: template_id,
      name: "Default Template",
      business_type: "restaurant",
      version: "1.0",
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now()
    }}
  end

  defp validate_template_compatibility(template, business) do
    if template.business_type == business.business_type do
      :ok
    else
      {:error, "Template is not compatible with business type"}
    end
  end

  defp apply_template_configuration(_business, _template) do
    # This would typically apply template configuration
    # For now, return :ok as stub implementation
    :ok
  end

  defp apply_template_services(_business, _template) do
    # This would typically apply template services
    # For now, return :ok as stub implementation
    :ok
  end

  defp apply_template_pricing(_business, _template) do
    # This would typically apply template pricing
    # For now, return :ok as stub implementation
    :ok
  end

  defp get_all_templates do
    # This would typically fetch all templates from the database
    # For now, return empty list as stub implementation
    []
  end

  defp get_templates_by_type(_business_type) do
    # This would typically fetch templates by type from the database
    # For now, return empty list as stub implementation
    []
  end

  defp get_completed_steps(_business_id) do
    # This would typically get completed steps from the database
    # For now, return empty list as stub implementation
    []
  end

  defp get_pending_steps(_business_id) do
    # This would typically get pending steps from the database
    # For now, return empty list as stub implementation
    []
  end

  defp calculate_overall_progress(business_id) do
    completed = length(get_completed_steps(business_id))
    total = completed + length(get_pending_steps(business_id))
    
    if total > 0 do
      (completed / total) * 100
    else
      0.0
    end
  end

  defp get_next_steps(_business_id) do
    # This would typically get next steps from the database
    # For now, return empty list as stub implementation
    []
  end

  defp get_estimated_completion_time(_business_id) do
    # This would typically calculate estimated completion time
    # For now, return nil as stub implementation
    nil
  end

  defp validate_step_eligibility(_step_name, _business) do
    # This would typically validate step eligibility
    # For now, return :ok as stub implementation
    :ok
  end

  defp process_step_data(_step_name, _step_data, _business) do
    # This would typically process step data
    # For now, return :ok as stub implementation
    :ok
  end

  defp mark_step_completed(_business_id, _step_name) do
    # This would typically mark step as completed in the database
    # For now, return :ok as stub implementation
    :ok
  end

  defp get_required_items(_business_type) do
    # This would typically get required items for the business type
    # For now, return empty list as stub implementation
    []
  end

  defp get_optional_items(_business_type) do
    # This would typically get optional items for the business type
    # For now, return empty list as stub implementation
    []
  end

  defp get_recommendations(_business_type) do
    # This would typically get recommendations for the business type
    # For now, return empty list as stub implementation
    []
  end

  defp get_step_dependencies(_business_type) do
    # This would typically get step dependencies for the business type
    # For now, return empty map as stub implementation
    %{}
  end

  defp get_estimated_setup_time(_business_type) do
    # This would typically get estimated setup time for the business type
    # For now, return 0 as stub implementation
    0
  end

  defp format_error(error) do
    case error do
      %{errors: errors} when is_list(errors) ->
        errors
        |> Enum.map(&format_error_message/1)
        |> Enum.join(", ")
      
      error when is_binary(error) ->
        error
      
      _ ->
        "An unknown error occurred"
    end
  end

  defp format_error_message({field, {message, _opts}}) do
    "#{field} #{message}"
  end

  defp format_error_message(%{message: message, field: field}) when is_atom(field) do
    "#{field} #{message}"
  end

  defp format_error_message(%{message: message}) do
    message
  end

  defp format_error_message(error) do
    inspect(error)
  end
end