# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     RivaAsh.Repo.insert!(%RivaAsh.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias RivaAsh.Resources.{Business, Section, Item}
alias RivaAsh.Domain

# Create sample businesses
sample_businesses = [
  %{name: "Tech Solutions Inc", description: "A technology consulting company"},
  %{name: "Green Energy Corp", description: "Renewable energy solutions provider"},
  %{name: "Creative Design Studio", description: "Digital design and marketing agency"}
]

IO.puts("Creating sample businesses...")

businesses = Enum.map(sample_businesses, fn business_attrs ->
  case Ash.create(Business, business_attrs, domain: Domain) do
    {:ok, business} ->
      IO.puts("Created business: #{business.name} (ID: #{business.id})")
      business
    {:error, error} ->
      IO.puts("Failed to create business '#{business_attrs.name}': #{inspect(error)}")
      nil
  end
end) |> Enum.filter(&(&1 != nil))

# Create sample sections for each business
sample_sections_data = [
  %{business_name: "Tech Solutions Inc", sections: [
    %{name: "Hardware", description: "Computer hardware and peripherals"},
    %{name: "Software", description: "Software applications and licenses"},
    %{name: "Services", description: "IT consulting and support services"}
  ]},
  %{business_name: "Green Energy Corp", sections: [
    %{name: "Solar", description: "Solar panel systems and components"},
    %{name: "Wind", description: "Wind energy equipment"},
    %{name: "Storage", description: "Energy storage solutions"}
  ]},
  %{business_name: "Creative Design Studio", sections: [
    %{name: "Web Design", description: "Website design and development"},
    %{name: "Branding", description: "Logo and brand identity design"},
    %{name: "Marketing", description: "Digital marketing campaigns"}
  ]}
]

IO.puts("Creating sample sections...")

sections = Enum.flat_map(sample_sections_data, fn %{business_name: business_name, sections: sections_list} ->
  business = Enum.find(businesses, &(&1.name == business_name))

  if business do
    Enum.map(sections_list, fn section_attrs ->
      section_attrs_with_business = Map.put(section_attrs, :business_id, business.id)

      case Ash.create(Section, section_attrs_with_business, domain: Domain) do
        {:ok, section} ->
          IO.puts("Created section: #{section.name} for #{business.name} (ID: #{section.id})")
          section
        {:error, error} ->
          IO.puts("Failed to create section '#{section_attrs.name}': #{inspect(error)}")
          nil
      end
    end) |> Enum.filter(&(&1 != nil))
  else
    []
  end
end)

# Create sample items for sections
sample_items_data = [
  %{section_name: "Hardware", items: ["Laptop Computer", "Desktop PC", "Monitor", "Keyboard", "Mouse"]},
  %{section_name: "Software", items: ["Office Suite", "Antivirus Software", "Design Software", "Database License"]},
  %{section_name: "Services", items: ["IT Consultation", "System Setup", "Technical Support", "Training"]},
  %{section_name: "Solar", items: ["Solar Panels", "Inverter", "Mounting System", "Monitoring System"]},
  %{section_name: "Wind", items: ["Wind Turbine", "Control System", "Power Converter", "Maintenance Kit"]},
  %{section_name: "Storage", items: ["Battery Pack", "Charge Controller", "Power Management System"]},
  %{section_name: "Web Design", items: ["Website Template", "Custom Design", "Mobile App Design", "E-commerce Site"]},
  %{section_name: "Branding", items: ["Logo Design", "Business Cards", "Letterhead", "Brand Guidelines"]},
  %{section_name: "Marketing", items: ["Social Media Campaign", "Email Marketing", "SEO Optimization", "Content Creation"]}
]

IO.puts("Creating sample items...")

Enum.each(sample_items_data, fn %{section_name: section_name, items: items_list} ->
  section = Enum.find(sections, &(&1.name == section_name))

  if section do
    Enum.each(items_list, fn item_name ->
      case Ash.create(Item, %{name: item_name, section_id: section.id}, domain: Domain) do
        {:ok, item} ->
          IO.puts("Created item: #{item.name} in section #{section.name} (ID: #{item.id})")
        {:error, error} ->
          IO.puts("Failed to create item '#{item_name}': #{inspect(error)}")
      end
    end)
  end
end)

IO.puts("Seeding completed!")
