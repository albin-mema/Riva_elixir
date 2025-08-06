defmodule Mix.Tasks.CreateSuperadmin do
  @moduledoc """
  Mix task to create a superadmin user for system oversight and GDPR compliance.

  This task creates a superadmin user who can:
  - Access the AshAdmin interface to view all system data
  - Manage user roles and permissions
  - Perform system-wide operations for compliance and oversight

  Usage:
    mix create_superadmin --email admin@example.com --name "System Admin" --password "secure_password"

  Or run interactively:
    mix create_superadmin
  """

  use Mix.Task
  alias RivaAsh.Accounts.User
  alias RivaAsh.Accounts

  @shortdoc "Creates a superadmin user for system oversight"

  def run(args) do
    Mix.Task.run("app.start")

    {opts, _args, _invalid} =
      OptionParser.parse(args,
        switches: [email: :string, name: :string, password: :string],
        aliases: [e: :email, n: :name, p: :password]
      )

    email = opts[:email] || prompt_for_email()
    name = opts[:name] || prompt_for_name()
    password = opts[:password] || prompt_for_password()

    case create_superadmin(email, name, password) do
      {:ok, user} ->
        Mix.shell().info("✅ Superadmin user created successfully!")
        Mix.shell().info("Email: #{user.email}")
        Mix.shell().info("Name: #{user.name}")
        Mix.shell().info("Role: #{user.role}")
        Mix.shell().info("")
        Mix.shell().info("🔐 You can now access the admin interface at: /admin")
        Mix.shell().info("🛡️  This user has full system access for GDPR compliance and oversight.")

      {:error, error} ->
        Mix.shell().error("❌ Failed to create superadmin user:")
        Mix.shell().error(inspect(error, pretty: true))
    end
  end

  defp create_superadmin(email, name, password) do
    # Try to create new superadmin user directly
    # If user exists, we'll get a unique constraint error and handle it
    case User
         |> Ash.Changeset.for_create(:register_with_password, %{
           email: email,
           name: name,
           password: password,
           role: "superadmin"
         })
         |> Ash.create(domain: Accounts, actor: %{role: "superadmin"}) do
      {:ok, user} ->
        {:ok, user}

      {:error, %Ash.Error.Invalid{errors: errors}} ->
        # Check if it's a unique constraint error (user already exists)
        case Enum.find(errors, fn error ->
               match?(%Ash.Error.Changes.InvalidAttribute{field: :email}, error) or
                 match?(%Ash.Error.Changes.InvalidChanges{fields: [:email]}, error)
             end) do
          nil ->
            {:error, %Ash.Error.Invalid{errors: errors}}

          _unique_error ->
            # User exists, try to promote them
            promote_existing_user(email)
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp promote_existing_user(email) do
    Mix.shell().info("User with email #{email} already exists. Promoting to superadmin...")

    # Find the user and promote them
    case Ash.read(User, domain: Accounts) do
      {:ok, users} ->
        case Enum.find(users, fn user -> user.email == email end) do
          nil ->
            {:error, "User not found"}

          existing_user ->
            existing_user
            |> Ash.Changeset.for_update(:promote_to_superadmin, %{})
            |> Ash.update(domain: Accounts, actor: %{role: "superadmin"})
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp prompt_for_email do
    email = Mix.shell().prompt("Enter superadmin email:")

    if String.contains?(email, "@") do
      String.trim(email)
    else
      Mix.shell().error("Invalid email format. Please try again.")
      prompt_for_email()
    end
  end

  defp prompt_for_name do
    name = Mix.shell().prompt("Enter superadmin name:")

    if String.trim(name) != "" do
      String.trim(name)
    else
      Mix.shell().error("Name cannot be empty. Please try again.")
      prompt_for_name()
    end
  end

  defp prompt_for_password do
    password = Mix.shell().prompt("Enter secure password (min 8 characters):")

    if String.length(String.trim(password)) >= 8 do
      String.trim(password)
    else
      Mix.shell().error("Password must be at least 8 characters. Please try again.")
      prompt_for_password()
    end
  end
end
