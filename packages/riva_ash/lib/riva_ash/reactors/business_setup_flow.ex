defmodule RivaAsh.Reactors.BusinessSetupFlow do
  @moduledoc """
  Comprehensive business setup reactor that creates a complete business
  infrastructure including plots, layouts, sections, item types, and pricing.

  This reactor handles the full business onboarding flow:
  1. Create business
  2. Create plot
  3. Create layout with grid system
  4. Create sections
  5. Create item types
  6. Set up pricing rules

  All operations are transactional - if any step fails, everything is rolled back.
  """

  use Reactor

  require Logger
  alias RivaAsh.Resources.{Business, Plot, Layout, Section, ItemType, Pricing}
  alias RivaAsh.Config.AppConfig

  @type business_info :: %{
          required(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:sections) => [%{name: String.t(), description: String.t()}],
          optional(:item_types) => [%{name: String.t(), description: String.t(), color: String.t()}],
          optional(:pricing) => %{optional(:default_daily_rate) => String.t(), optional(:currency) => String.t()},
          optional(:item_pricing) => %{String.t() => String.t()}
        }

  @type plot_details :: %{
          optional(:name) => String.t(),
          optional(:description) => String.t(),
          optional(:total_area) => float(),
          optional(:location) => String.t(),
          optional(:layout_name) => String.t(),
          optional(:layout_description) => String.t(),
          optional(:grid_rows) => integer(),
          optional(:grid_columns) => integer()
        }

  @type result :: %{
          business: Business.t(),
          plot: Plot.t(),
          layout: Layout.t(),
          sections: [Section.t()],
          item_types: [ItemType.t()],
          pricing_rules: [Pricing.t()]
        }

  @spec create_business(map(), integer()) :: {:ok, Business.t()} | {:error, String.t()}
  @spec create_plot(integer(), map()) :: {:ok, Plot.t()} | {:error, String.t()}
  @spec create_layout(integer(), map()) :: {:ok, Layout.t()} | {:error, String.t()}
  @spec create_sections(integer(), business_info()) :: {:ok, [Section.t()]} | {:error, String.t()}
  @spec create_item_types(integer(), business_info()) :: {:ok, [ItemType.t()]} | {:error, String.t()}
  @spec create_pricing_rules(integer(), [ItemType.t()], business_info()) :: {:ok, [Pricing.t()]} | {:error, String.t()}
  @spec build_result(map()) :: {:ok, result()} | {:error, String.t()}

  # Define the reactor inputs
  input(:business_info)
  input(:plot_details)
  input(:owner_id)

  # Step 1: Create the business
  step :create_business do
    argument(:business_info, input(:business_info))
    argument(:owner_id, input(:owner_id))

    run(fn %{business_info: info, owner_id: owner_id}, _context ->
      Logger.info("Starting business creation with name: #{info.name}")

      with :ok <- validate_business_info(info),
           {:ok, business} <- do_create_business(info, owner_id) do
        Logger.info("Business created successfully: #{business.id}")
        {:ok, business}
      else
        {:error, reason} ->
          Logger.error("Business creation failed: #{inspect(reason)}")
          {:error, reason}
      end
    end)

    compensate(fn business, _context ->
      Logger.warning("Compensating business creation for business: #{business.id}")
      Business.destroy!(business, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Helper functions for business creation
  defp validate_business_info(%{name: name}) when is_binary(name) and byte_size(name) > 0 do
    Logger.debug("Business name validation passed: #{name}")
    :ok
  end

  defp validate_business_info(_info) do
    Logger.debug("Business name validation failed: missing or invalid name")
    {:error, "Business name is required"}
  end

  defp do_create_business(%{name: name, description: description}, owner_id) do
    Logger.debug("Creating business with name: #{name}, owner_id: #{owner_id}")

    business_attrs = %{
      name: name,
      description: description || "",
      owner_id: owner_id
    }

    result =
      Business
      |> Ash.Changeset.for_create(:create, business_attrs)
      |> Ash.create(domain: RivaAsh.Domain)

    case result do
      {:ok, business} ->
        Logger.debug("Business created successfully with ID: #{business.id}")
        {:ok, business}

      {:error, changeset} ->
        Logger.error("Business creation failed: #{inspect(changeset)}")
        {:error, "Failed to create business: #{format_changeset_errors(changeset)}"}
    end
  end

  defp do_create_business(%{name: name}, owner_id) do
    Logger.debug("Creating business with default description")
    do_create_business(%{name: name, description: ""}, owner_id)
  end

  # Helper function to format changeset errors
  defp format_changeset_errors(changeset) do
    changeset
    |> Ash.Changeset.traverse_errors(fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
    |> Enum.map(fn {field, errors} -> "#{field}: #{Enum.join(errors, ", ")}" end)
    |> Enum.join("; ")
  end

  # Step 2: Create the plot
  step :create_plot do
    argument(:business_id, result(:create_business, [:id]))
    argument(:plot_details, input(:plot_details))

    run(fn %{business_id: business_id, plot_details: details}, _context ->
      Logger.info("Creating plot for business: #{business_id}")

      plot_attrs = %{
        name: details[:name] || "Main Plot",
        description: details[:description] || "Primary business plot",
        business_id: business_id,
        total_area: details[:total_area] || 1000.0,
        location: details[:location] || "Main Location"
      }

      result =
        Plot
        |> Ash.Changeset.for_create(:create, plot_attrs)
        |> Ash.create(domain: RivaAsh.Domain)

      case result do
        {:ok, plot} ->
          Logger.info("Plot created successfully: #{plot.id}")
          {:ok, plot}

        {:error, changeset} ->
          Logger.error("Plot creation failed: #{inspect(changeset)}")
          {:error, "Failed to create plot: #{format_changeset_errors(changeset)}"}
      end
    end)

    compensate(fn plot, _context ->
      Logger.warning("Compensating plot creation for plot: #{plot.id}")
      Plot.destroy!(plot, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 3: Create the layout
  step :create_layout do
    argument(:plot_id, result(:create_plot, [:id]))
    argument(:plot_details, input(:plot_details))

    run(fn %{plot_id: plot_id, plot_details: details}, _context ->
      Logger.info("Creating layout for plot: #{plot_id}")

      layout_attrs = %{
        name: details[:layout_name] || "Main Layout",
        description: details[:layout_description] || "Primary layout for the plot",
        plot_id: plot_id,
        grid_rows: details[:grid_rows] || 10,
        grid_columns: details[:grid_columns] || 10
      }

      result =
        Layout
        |> Ash.Changeset.for_create(:create, layout_attrs)
        |> Ash.create(domain: RivaAsh.Domain)

      case result do
        {:ok, layout} ->
          Logger.info("Layout created successfully: #{layout.id}")
          {:ok, layout}

        {:error, changeset} ->
          Logger.error("Layout creation failed: #{inspect(changeset)}")
          {:error, "Failed to create layout: #{format_changeset_errors(changeset)}"}
      end
    end)

    compensate(fn layout, _context ->
      Logger.warning("Compensating layout creation for layout: #{layout.id}")
      Layout.destroy!(layout, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 4: Create default sections
  step :create_sections do
    argument(:business_id, result(:create_business, [:id]))
    argument(:business_info, input(:business_info))

    run(fn %{business_id: business_id, business_info: info}, _context ->
      Logger.info("Creating sections for business: #{business_id}")
      sections = info[:sections] || [%{name: "Main Section", description: "Default section"}]

      created_sections =
        Enum.map(sections, fn section_info ->
          Logger.debug("Creating section: #{section_info.name}")

          result =
            Section
            |> Ash.Changeset.for_create(:create, %{
              name: section_info.name,
              description: section_info.description,
              business_id: business_id
            })
            |> Ash.create(domain: RivaAsh.Domain)

          case result do
            {:ok, section} ->
              Logger.debug("Section created successfully: #{section.id}")
              section

            {:error, changeset} ->
              Logger.error("Section creation failed for #{section_info.name}: #{inspect(changeset)}")
              raise "Failed to create section: #{format_changeset_errors(changeset)}"
          end
        end)

      Logger.info("Successfully created #{length(created_sections)} sections")
      {:ok, created_sections}
    end)

    compensate(fn sections, _context ->
      Logger.warning("Compensating section creation for #{length(sections)} sections")

      Enum.each(sections, fn section ->
        Logger.debug("Destroying section: #{section.id}")
        Section.destroy!(section, domain: RivaAsh.Domain)
      end)

      :ok
    end)
  end

  # Step 5: Create item types
  step :create_item_types do
    argument(:business_id, result(:create_business, [:id]))
    argument(:business_info, input(:business_info))

    run(fn %{business_id: business_id, business_info: info}, _context ->
      Logger.info("Creating item types for business: #{business_id}")

      item_types =
        info[:item_types] ||
          [
            %{name: "Standard Spot", description: "Standard reservation spot", color: "#3B82F6"},
            %{name: "Premium Spot", description: "Premium reservation spot", color: "#10B981"}
          ]

      created_types =
        Enum.map(item_types, fn type_info ->
          Logger.debug("Creating item type: #{type_info.name}")

          result =
            ItemType
            |> Ash.Changeset.for_create(:create, %{
              name: type_info.name,
              description: type_info.description,
              color: type_info[:color] || "#6B7280",
              business_id: business_id
            })
            |> Ash.create(domain: RivaAsh.Domain)

          case result do
            {:ok, item_type} ->
              Logger.debug("Item type created successfully: #{item_type.id}")
              item_type

            {:error, changeset} ->
              Logger.error("Item type creation failed for #{type_info.name}: #{inspect(changeset)}")
              raise "Failed to create item type: #{format_changeset_errors(changeset)}"
          end
        end)

      Logger.info("Successfully created #{length(created_types)} item types")
      {:ok, created_types}
    end)

    compensate(fn item_types, _context ->
      Logger.warning("Compensating item type creation for #{length(item_types)} item types")

      Enum.each(item_types, fn item_type ->
        Logger.debug("Destroying item type: #{item_type.id}")
        ItemType.destroy!(item_type, domain: RivaAsh.Domain)
      end)

      :ok
    end)
  end

  # Step 6: Create pricing rules
  step :create_pricing_rules do
    argument(:business_id, result(:create_business, [:id]))
    argument(:item_types, result(:create_item_types))
    argument(:business_info, input(:business_info))

    run(fn %{business_id: business_id, item_types: item_types, business_info: info}, _context ->
      Logger.info("Creating pricing rules for business: #{business_id}")
      pricing_info = info[:pricing] || %{default_daily_rate: "50.00", currency: "USD"}
      item_pricing = info[:item_pricing] || %{}

      created_pricing =
        Enum.map(item_types, fn item_type ->
          daily_rate = item_pricing[item_type.name] || pricing_info.default_daily_rate
          Logger.debug("Creating pricing for item type: #{item_type.name} at rate #{daily_rate}")

          result =
            Pricing
            |> Ash.Changeset.for_create(:create, %{
              business_id: business_id,
              item_type_id: item_type.id,
              price_per_day: Decimal.new(daily_rate),
              currency: pricing_info[:currency] || "USD",
              effective_from: Date.utc_today()
            })
            |> Ash.create(domain: RivaAsh.Domain)

          case result do
            {:ok, pricing} ->
              Logger.debug("Pricing rule created successfully: #{pricing.id}")
              pricing

            {:error, changeset} ->
              Logger.error("Pricing rule creation failed for #{item_type.name}: #{inspect(changeset)}")
              raise "Failed to create pricing rule: #{format_changeset_errors(changeset)}"
          end
        end)

      Logger.info("Successfully created #{length(created_pricing)} pricing rules")
      {:ok, created_pricing}
    end)

    compensate(fn pricing_rules, _context ->
      Logger.warning("Compensating pricing rule creation for #{length(pricing_rules)} rules")

      Enum.each(pricing_rules, fn pricing ->
        Logger.debug("Destroying pricing rule: #{pricing.id}")
        Pricing.destroy!(pricing, domain: RivaAsh.Domain)
      end)

      :ok
    end)
  end

  # Return a comprehensive result with all created resources
  step :build_result do
    argument(:business, result(:create_business))
    argument(:plot, result(:create_plot))
    argument(:layout, result(:create_layout))
    argument(:sections, result(:create_sections))
    argument(:item_types, result(:create_item_types))
    argument(:pricing_rules, result(:create_pricing_rules))

    run(fn args, _context ->
      Logger.info("Building final result for business setup flow")

      result = %{
        business: args.business,
        plot: args.plot,
        layout: args.layout,
        sections: args.sections,
        item_types: args.item_types,
        pricing_rules: args.pricing_rules
      }

      Logger.info("Business setup flow completed successfully")
      {:ok, result}
    end)
  end

  return(:build_result)
end
