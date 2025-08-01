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

  alias RivaAsh.Resources.{Business, Plot, Layout, Section, ItemType, Pricing}

  # Define the reactor inputs
  input(:business_info)
  input(:plot_details)
  input(:owner_id)

  # Step 1: Create the business
  step :create_business do
    argument(:business_info, input(:business_info))
    argument(:owner_id, input(:owner_id))

    run(fn %{business_info: info, owner_id: owner_id}, _context ->
      Business
      |> Ash.Changeset.for_create(:create, %{
        name: info.name,
        description: info.description,
        owner_id: owner_id
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn business, _context ->
      Business.destroy!(business, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 2: Create the plot
  step :create_plot do
    argument(:business_id, result(:create_business, [:id]))
    argument(:plot_details, input(:plot_details))

    run(fn %{business_id: business_id, plot_details: details}, _context ->
      Plot
      |> Ash.Changeset.for_create(:create, %{
        name: details[:name] || "Main Plot",
        description: details[:description] || "Primary business plot",
        business_id: business_id,
        total_area: details[:total_area] || 1000.0,
        location: details[:location] || "Main Location"
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn plot, _context ->
      Plot.destroy!(plot, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 3: Create the layout
  step :create_layout do
    argument(:plot_id, result(:create_plot, [:id]))
    argument(:plot_details, input(:plot_details))

    run(fn %{plot_id: plot_id, plot_details: details}, _context ->
      Layout
      |> Ash.Changeset.for_create(:create, %{
        name: details[:layout_name] || "Main Layout",
        description: details[:layout_description] || "Primary layout for the plot",
        plot_id: plot_id,
        grid_rows: details[:grid_rows] || 10,
        grid_columns: details[:grid_columns] || 10
      })
      |> Ash.create(domain: RivaAsh.Domain)
    end)

    compensate(fn layout, _context ->
      Layout.destroy!(layout, domain: RivaAsh.Domain)
      :ok
    end)
  end

  # Step 4: Create default sections
  step :create_sections do
    argument(:business_id, result(:create_business, [:id]))
    argument(:business_info, input(:business_info))

    run(fn %{business_id: business_id, business_info: info}, _context ->
      sections = info[:sections] || [%{name: "Main Section", description: "Default section"}]

      created_sections =
        Enum.map(sections, fn section_info ->
          {:ok, section} =
            Section
            |> Ash.Changeset.for_create(:create, %{
              name: section_info.name,
              description: section_info.description,
              business_id: business_id
            })
            |> Ash.create(domain: RivaAsh.Domain)

          section
        end)

      {:ok, created_sections}
    end)

    compensate(fn sections, _context ->
      Enum.each(sections, fn section ->
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
      item_types =
        info[:item_types] ||
          [
            %{name: "Standard Spot", description: "Standard reservation spot", color: "#3B82F6"},
            %{name: "Premium Spot", description: "Premium reservation spot", color: "#10B981"}
          ]

      created_types =
        Enum.map(item_types, fn type_info ->
          {:ok, item_type} =
            ItemType
            |> Ash.Changeset.for_create(:create, %{
              name: type_info.name,
              description: type_info.description,
              color: type_info[:color] || "#6B7280",
              business_id: business_id
            })
            |> Ash.create(domain: RivaAsh.Domain)

          item_type
        end)

      {:ok, created_types}
    end)

    compensate(fn item_types, _context ->
      Enum.each(item_types, fn item_type ->
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
      pricing_info = info[:pricing] || %{default_daily_rate: "50.00", currency: "USD"}
      item_pricing = info[:item_pricing] || %{}

      created_pricing =
        Enum.map(item_types, fn item_type ->
          daily_rate = item_pricing[item_type.name] || pricing_info.default_daily_rate

          {:ok, pricing} =
            Pricing
            |> Ash.Changeset.for_create(:create, %{
              business_id: business_id,
              item_type_id: item_type.id,
              price_per_day: Decimal.new(daily_rate),
              currency: pricing_info[:currency] || "USD",
              effective_from: Date.utc_today()
            })
            |> Ash.create(domain: RivaAsh.Domain)

          pricing
        end)

      {:ok, created_pricing}
    end)

    compensate(fn pricing_rules, _context ->
      Enum.each(pricing_rules, fn pricing ->
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
      result = %{
        business: args.business,
        plot: args.plot,
        layout: args.layout,
        sections: args.sections,
        item_types: args.item_types,
        pricing_rules: args.pricing_rules
      }

      {:ok, result}
    end)
  end

  return(:build_result)
end
