defmodule RivaAsh.Resources.ClientTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Resources.Client
  import RivaAsh.Factory

  describe "create/1" do
    test "creates a client with valid attributes" do
      attrs = %{
        name: "John Doe",
        email: "john@example.com",
        phone: "+1234567890",
        is_registered: true
      }

      assert {:ok, client} = Client.create(attrs)
      assert client.name == "John Doe"
      assert client.email == "john@example.com"
      assert client.phone == "+1234567890"
      assert client.is_registered == true
    end

    test "validates required fields" do
      assert {:error, %{errors: errors}} = Client.create(%{})
      assert "is required" in errors_on(errors, :name)
    end

    test "validates email format for registered clients" do
      attrs = %{
        name: "John Doe",
        email: "invalid-email",
        is_registered: true
      }

      assert {:error, %{errors: errors}} = Client.create(attrs)
      assert "has invalid format" in errors_on(errors, :email)
    end
  end

  describe "by_email/1" do
    test "finds client by email (case insensitive)" do
      {:ok, _} = Client.create(%{name: "John Doe", email: "john@example.com", is_registered: true})
      
      assert {:ok, client} = Client.by_email("JOHN@example.com")
      assert client.name == "John Doe"
    end
  end

  describe "registered/0" do
    test "returns only registered clients" do
      {:ok, _} = Client.create(%{name: "John", email: "john@example.com", is_registered: true})
      {:ok, _} = Client.create(%{name: "Jane", email: "jane@example.com", is_registered: true})
      {:ok, _} = Client.create(%{name: "Guest", is_registered: false})

      assert {:ok, clients} = Client.registered()
      assert length(clients) == 2
      assert Enum.all?(clients, & &1.is_registered)
    end
  end

  describe "unregistered/0" do
    test "returns only unregistered clients" do
      {:ok, _} = Client.create(%{name: "John", email: "john@example.com", is_registered: true})
      {:ok, _} = Client.create(%{name: "Guest 1", is_registered: false})
      {:ok, _} = Client.create(%{name: "Guest 2", is_registered: false})

      assert {:ok, clients} = Client.unregistered()
      assert length(clients) == 2
      assert Enum.all?(clients, &(!&1.is_registered))
    end
  end
end
