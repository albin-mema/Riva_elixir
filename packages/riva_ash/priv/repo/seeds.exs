# Script for populating the database with realistic test data using Faker
#
#     mix run priv/repo/seeds.exs
#
# This script creates a comprehensive set of test data including:
# - Admin and regular users
# - Multiple businesses with realistic data
# - Employees, clients, items, and reservations
# - Proper relationships and realistic quantities

alias RivaAsh.Resources.{Business, Section, Item, Employee, Client, Reservation, ItemType, Plot, Layout}
alias RivaAsh.Accounts.User
alias RivaAsh.Domain
import Ash.Expr
require Ash.Query

# Configure Faker locale
Faker.start()

# Configuration for data generation
config = %{
  users: %{admin: 1, regular: 5},
  businesses: %{count: 3..8},
  employees_per_business: %{count: 2..8},
  clients_per_business: %{count: 5..15},
  sections_per_business: %{count: 2..5},
  items_per_section: %{count: 3..12},
  reservations_per_client: %{count: 0..3}
}

# Helper functions for seeding
defmodule SeedHelpers do
  def create_or_find_user(email, attrs) do
    case User |> Ash.Query.filter(expr(email == ^email)) |> Ash.read_one(action: :seed_read) do
      {:ok, user} when not is_nil(user) ->
        IO.puts("User already exists: #{user.email} (ID: #{user.id})")
        user

      {:ok, nil} ->
        case User
             |> Ash.Changeset.for_create(:register_with_password, attrs)
             |> Ash.create() do
          {:ok, user} ->
            IO.puts("Created user: #{user.email} (ID: #{user.id})")
            user

          {:error, error} ->
            IO.puts("Failed to create user '#{email}': #{inspect(error)}")
            nil
        end

      {:error, error} ->
        IO.puts("Error checking for user '#{email}': #{inspect(error)}")
        nil
    end
  end

  def random_count(range), do: Enum.random(range)

  def create_business(owner_id, actor) do
    company_name = Faker.Company.name()

    attrs = %{
      name: company_name,
      description: Faker.Company.catch_phrase(),
      owner_id: owner_id
    }

    case Business |> Ash.Changeset.for_create(:create, attrs, actor: actor) |> Ash.create() do
      {:ok, business} ->
        IO.puts("Created business: #{business.name} (ID: #{business.id})")
        business
      {:error, error} ->
        IO.puts("Failed to create business '#{company_name}': #{inspect(error)}")
        nil
    end
  end
end

IO.puts("🌱 Starting comprehensive database seeding with Faker...")

# Create admin user
IO.puts("Creating admin user...")
admin_user = SeedHelpers.create_or_find_user("admin@example.com", %{
  email: "admin@example.com",
  name: "Admin User",
  role: "admin",
  password: "admin123456"
})

unless admin_user do
  IO.puts("❌ Failed to create admin user. Exiting.")
  System.halt(1)
end

# Create regular users
IO.puts("Creating regular users...")
regular_users = Enum.map(1..config.users.regular, fn i ->
  email = "user#{i}@example.com"
  SeedHelpers.create_or_find_user(email, %{
    email: email,
    name: Faker.Person.name(),
    role: "user",
    password: "password123"
  })
end) |> Enum.filter(&(&1 != nil))

all_users = [admin_user | regular_users]

# Create businesses
IO.puts("Creating businesses...")
business_count = SeedHelpers.random_count(config.businesses.count)
businesses = Enum.map(1..business_count, fn _i ->
  owner = Enum.random(all_users)
  SeedHelpers.create_business(owner.id, admin_user)
end) |> Enum.filter(&(&1 != nil))

IO.puts("✅ Created #{length(businesses)} businesses")

# Create employees for each business
IO.puts("Creating employees...")
all_employees = Enum.flat_map(businesses, fn business ->
  employee_count = SeedHelpers.random_count(config.employees_per_business.count)

  Enum.map(1..employee_count, fn _i ->
    first_name = Faker.Person.first_name()
    last_name = Faker.Person.last_name()

    attrs = %{
      first_name: first_name,
      last_name: last_name,
      email: "#{String.downcase(first_name)}.#{String.downcase(last_name)}@#{String.downcase(String.replace(business.name, " ", ""))}.com",
      role: Enum.random([:manager, :staff, :admin]),
      phone: Faker.Phone.EnUs.phone(),
      business_id: business.id
    }

    case Employee |> Ash.Changeset.for_create(:create, attrs, actor: admin_user) |> Ash.create() do
      {:ok, employee} ->
        employee
      {:error, _error} ->
        nil
    end
  end) |> Enum.filter(&(&1 != nil))
end)

IO.puts("✅ Created #{length(all_employees)} employees")

# Create clients for each business
IO.puts("Creating clients...")
all_clients = Enum.flat_map(businesses, fn business ->
  client_count = SeedHelpers.random_count(config.clients_per_business.count)

  Enum.map(1..client_count, fn _i ->
    attrs = %{
      name: Faker.Person.name(),
      email: Faker.Internet.email(),
      phone: "#{Enum.random(200..999)}-#{Enum.random(200..999)}-#{Enum.random(1000..9999)}",
      business_id: business.id
    }

    case Client |> Ash.Changeset.for_create(:create, attrs, actor: admin_user) |> Ash.create() do
      {:ok, client} ->
        client
      {:error, _error} ->
        nil
    end
  end) |> Enum.filter(&(&1 != nil))
end)

IO.puts("✅ Created #{length(all_clients)} clients")

# Create sections for each business
IO.puts("Creating sections...")
all_sections = Enum.flat_map(businesses, fn business ->
  section_count = SeedHelpers.random_count(config.sections_per_business.count)

  # Generate realistic section names based on business type
  section_types = [
    "Sales", "Marketing", "Operations", "Customer Service", "Administration",
    "Finance", "Human Resources", "IT Support", "Research & Development",
    "Quality Assurance", "Logistics", "Procurement"
  ]

  Enum.map(1..section_count, fn _i ->
    section_name = Enum.random(section_types)

    attrs = %{
      name: section_name,
      description: "#{section_name} department for #{business.name}",
      business_id: business.id
    }

    case Section |> Ash.Changeset.for_create(:create, attrs, actor: admin_user) |> Ash.create() do
      {:ok, section} ->
        section
      {:error, _error} ->
        nil
    end
  end) |> Enum.filter(&(&1 != nil))
end)

IO.puts("✅ Created #{length(all_sections)} sections")

# Create items for each section
IO.puts("Creating items...")
all_items = Enum.flat_map(all_sections, fn section ->
  item_count = SeedHelpers.random_count(config.items_per_section.count)

  Enum.map(1..item_count, fn _i ->
    attrs = %{
      name: Faker.Commerce.product_name(),
      description: Faker.Lorem.sentence(),
      section_id: section.id
    }

    case Item |> Ash.Changeset.for_create(:create, attrs, actor: admin_user) |> Ash.create() do
      {:ok, item} ->
        item
      {:error, _error} ->
        nil
    end
  end) |> Enum.filter(&(&1 != nil))
end)

IO.puts("✅ Created #{length(all_items)} items")

# Create some reservations
IO.puts("Creating reservations...")

all_reservations = Enum.flat_map(all_clients, fn client ->
  reservation_count_for_client = SeedHelpers.random_count(config.reservations_per_client.count)

  if reservation_count_for_client > 0 and length(all_items) > 0 do
    Enum.map(1..reservation_count_for_client, fn _i ->
      item = Enum.random(all_items)
      start_time = Faker.DateTime.between(~N[2024-01-01 09:00:00], ~N[2024-12-31 17:00:00])
      end_time = NaiveDateTime.add(start_time, Enum.random([3600, 7200, 10800]), :second) # 1-3 hours

      attrs = %{
        client_id: client.id,
        item_id: item.id,
        start_time: start_time,
        end_time: end_time,
        status: Enum.random([:pending, :confirmed, :completed, :cancelled]),
        notes: Faker.Lorem.sentence()
      }

      case Reservation |> Ash.Changeset.for_create(:create, attrs, actor: admin_user) |> Ash.create() do
        {:ok, reservation} ->
          reservation
        {:error, _error} ->
          nil
      end
    end) |> Enum.filter(&(&1 != nil))
  else
    []
  end
end)

IO.puts("✅ Created #{length(all_reservations)} reservations")

# Summary
IO.puts("\n🎉 Seeding completed successfully!")
IO.puts("📊 Summary:")
IO.puts("  👥 Users: #{length(all_users)} (1 admin, #{length(regular_users)} regular)")
IO.puts("  🏢 Businesses: #{length(businesses)}")
IO.puts("  👔 Employees: #{length(all_employees)}")
IO.puts("  👤 Clients: #{length(all_clients)}")
IO.puts("  📂 Sections: #{length(all_sections)}")
IO.puts("  📦 Items: #{length(all_items)}")
IO.puts("  📅 Reservations: #{length(all_reservations)}")
IO.puts("\n✨ Your database is now populated with realistic test data!")
