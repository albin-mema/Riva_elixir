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
alias RivaAsh.Accounts.User
alias RivaAsh.Domain
import Ash.Expr
require Ash.Query

IO.puts("Creating admin user...")

# Check if admin user already exists by email
admin_user = case User |> Ash.Query.filter(expr(email == "admin@example.com")) |> Ash.read_one() do
  {:ok, user} when not is_nil(user) ->
    IO.puts("Admin user already exists: #{user.email} (ID: #{user.id})")
    user

  {:ok, nil} ->
    # Create the admin user
    case User
         |> Ash.Changeset.for_create(:register_with_password, %{
           email: "admin@example.com",
           name: "Admin User",
           role: :admin,
           password: "admin123456"
         })
         |> Ash.create() do
      {:ok, user} ->
        IO.puts("Created admin user: #{user.email} (ID: #{user.id})")
        user

      {:error, error} ->
        IO.puts("Failed to create admin user: #{inspect(error)}")
        System.halt(1)
    end

  {:error, error} ->
    IO.puts("Error checking for admin user: #{inspect(error)}")
    System.halt(1)
end

# Create or get sample businesses
sample_businesses = [
  %{name: "Tech Solutions Inc", description: "A technology consulting company"},
  %{name: "Green Energy Corp", description: "Renewable energy solutions provider"},
  %{name: "Creative Design Studio", description: "Digital design and marketing agency"}
]

IO.puts("Creating/finding sample businesses...")

businesses = Enum.map(sample_businesses, fn business_attrs ->
  business_name = business_attrs.name
  
  # Check if business already exists
  case Business |> Ash.Query.filter(expr(name == ^business_name)) |> Ash.read_one() do
    {:ok, existing_business} when not is_nil(existing_business) ->
      IO.puts("Business already exists: #{existing_business.name} (ID: #{existing_business.id})")
      existing_business
    
    {:ok, nil} ->
      # Create new business
      business_attrs_with_owner = Map.put(business_attrs, :owner_id, admin_user.id)
      case Business |> Ash.Changeset.for_create(:create, business_attrs_with_owner, actor: admin_user) |> Ash.create() do
        {:ok, business} ->
          IO.puts("Created business: #{business.name} (ID: #{business.id})")
          business
        {:error, error} ->
          IO.puts("Failed to create business '#{business_attrs.name}': #{inspect(error)}")
          nil
      end
    
    {:error, error} ->
      IO.puts("Error checking for business '#{business_name}': #{inspect(error)}")
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

      case Section |> Ash.Changeset.for_create(:create, section_attrs_with_business, actor: admin_user) |> Ash.create() do
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
      case Item |> Ash.Changeset.for_create(:create, %{name: item_name, section_id: section.id}, actor: admin_user) |> Ash.create() do
        {:ok, item} ->
          IO.puts("Created item: #{item.name} in section #{section.name} (ID: #{item.id})")
        {:error, error} ->
          IO.puts("Failed to create item '#{item_name}': #{inspect(error)}")
      end
    end)
  end
end)

# Create sample employees for each business
alias RivaAsh.Resources.Employee

sample_employees = [
  %{first_name: "John", last_name: "Doe", email: "john.doe@techsolutions.com", role: :manager, phone: "+1-555-0101"},
  %{first_name: "Jane", last_name: "Smith", email: "jane.smith@techsolutions.com", role: :staff, phone: "+1-555-0102"},
  %{first_name: "Mike", last_name: "Johnson", email: "mike.johnson@greenenergy.com", role: :manager, phone: "+1-555-0201"},
  %{first_name: "Sarah", last_name: "Williams", email: "sarah.williams@greenenergy.com", role: :staff, phone: "+1-555-0202"},
  %{first_name: "David", last_name: "Brown", email: "david.brown@creativedesign.com", role: :manager, phone: "+1-555-0301"},
  %{first_name: "Lisa", last_name: "Davis", email: "lisa.davis@creativedesign.com", role: :staff, phone: "+1-555-0302"}
]

IO.puts("Creating sample employees...")

# Map business names to business IDs
business_map = %{
  "Tech Solutions Inc" => Enum.find(businesses, &(&1.name == "Tech Solutions Inc")),
  "Green Energy Corp" => Enum.find(businesses, &(&1.name == "Green Energy Corp")),
  "Creative Design Studio" => Enum.find(businesses, &(&1.name == "Creative Design Studio"))
}

Enum.each(sample_employees, fn employee_attrs ->
  # Determine business based on email domain
  business = cond do
    String.contains?(employee_attrs.email, "techsolutions") -> business_map["Tech Solutions Inc"]
    String.contains?(employee_attrs.email, "greenenergy") -> business_map["Green Energy Corp"]
    String.contains?(employee_attrs.email, "creativedesign") -> business_map["Creative Design Studio"]
    true -> nil
  end

  if business do
    # Check if employee already exists
    case Employee |> Ash.Query.filter(expr(email == ^employee_attrs.email)) |> Ash.read_one() do
      {:ok, existing_employee} when not is_nil(existing_employee) ->
        IO.puts("Employee already exists: #{existing_employee.first_name} #{existing_employee.last_name} (#{existing_employee.email})")
      
      {:ok, nil} ->
        # Create new employee
        employee_attrs_with_business = Map.put(employee_attrs, :business_id, business.id)
        case Employee |> Ash.Changeset.for_create(:create, employee_attrs_with_business, actor: admin_user) |> Ash.create() do
          {:ok, employee} ->
            IO.puts("Created employee: #{employee.first_name} #{employee.last_name} (#{employee.email}) for #{business.name}")
          {:error, error} ->
            IO.puts("Failed to create employee '#{employee_attrs.first_name} #{employee_attrs.last_name}': #{inspect(error)}")
        end
      
      {:error, error} ->
        IO.puts("Error checking for employee '#{employee_attrs.email}': #{inspect(error)}")
    end
  else
    IO.puts("Could not find business for employee: #{employee_attrs.first_name} #{employee_attrs.last_name}")
  end
end)

IO.puts("Seeding completed!")
