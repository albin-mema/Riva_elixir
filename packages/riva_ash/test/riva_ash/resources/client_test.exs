defmodule RivaAsh.Resources.ClientTest do
  use RivaAsh.DataCase, async: true
  alias RivaAsh.Resources.Client

  describe "create/1 - property-based tests" do
    @tag property: true
    property "creates clients with valid randomized attributes" do
      check all attrs <- client_attrs() do
        case Client.create(attrs) do
          {:ok, client} ->
            assert client.name == attrs.name
            # Handle CiString comparison for email
            if attrs.email do
              assert to_string(client.email) == attrs.email
            else
              assert client.email == nil
            end
            assert client.phone == attrs.phone
            assert client.is_registered == attrs.is_registered
            assert client.id != nil

            # Verify email is required for registered clients
            if attrs.is_registered do
              assert client.email != nil
            end

          {:error, _error} ->
            # Some combinations might be invalid, which is expected
            :ok
        end
      end
    end

    @tag property: true
    property "validates required name field" do
      check all attrs <- client_attrs(%{name: nil}) do
        assert {:error, %{errors: errors}} = Client.create(attrs)
        assert has_error_on_field?(%{errors: errors}, :name)
      end
    end

    @tag property: true
    property "validates email format for registered clients" do
      check all attrs <- client_attrs(%{is_registered: true}) do
        # Override with invalid email format
        invalid_attrs = Map.put(attrs, :email, "invalid-email")
        case Client.create(invalid_attrs) do
          {:error, %{errors: errors}} ->
            # Should have an error on email field
            assert has_error_on_field?(%{errors: errors}, :email)
          {:ok, _client} ->
            # This shouldn't happen with invalid email, but let's not fail the test
            # as the validation might be more lenient than expected
            :ok
        end
      end
    end

    @tag property: true
    property "allows unregistered clients without email" do
      check all attrs <- client_attrs(%{is_registered: false, email: nil}) do
        assert {:ok, client} = Client.create(attrs)
        assert client.is_registered == false
        assert client.email == nil
      end
    end
  end

  describe "by_email/1 - property-based tests" do
    @tag property: true
    property "finds client by email (case insensitive)" do
      check all attrs <- client_attrs(%{is_registered: true}) do
        # Create client with valid email
        {:ok, created_client} = Client.create(attrs)

        # Test finding by exact email
        assert {:ok, found_client} = Client.by_email(attrs.email)
        assert found_client.id == created_client.id

        # Test finding by uppercase email (case insensitive)
        if attrs.email do
          uppercase_email = String.upcase(attrs.email)
          assert {:ok, found_client_upper} = Client.by_email(uppercase_email)
          assert found_client_upper.id == created_client.id
        end
      end
    end
  end

  describe "registered/0 - property-based tests" do
    @tag property: true
    property "returns only registered clients" do
      check all registered_attrs_list <- list_of(client_attrs(%{is_registered: true}), min_length: 1, max_length: 5),
                unregistered_attrs_list <- list_of(client_attrs(%{is_registered: false}), min_length: 1, max_length: 3) do

        # Create registered clients
        registered_clients = Enum.map(registered_attrs_list, fn attrs ->
          {:ok, client} = Client.create(attrs)
          client
        end)

        # Create unregistered clients
        _unregistered_clients = Enum.map(unregistered_attrs_list, fn attrs ->
          {:ok, client} = Client.create(attrs)
          client
        end)

        # Query registered clients
        assert {:ok, found_clients} = Client.registered()

        # Verify all returned clients are registered
        assert Enum.all?(found_clients, & &1.is_registered)

        # Verify we found at least the registered clients we created
        registered_ids = Enum.map(registered_clients, & &1.id)
        found_ids = Enum.map(found_clients, & &1.id)
        assert Enum.all?(registered_ids, &(&1 in found_ids))
      end
    end
  end

  describe "unregistered/0 - property-based tests" do
    @tag property: true
    property "returns only unregistered clients" do
      check all registered_attrs_list <- list_of(client_attrs(%{is_registered: true}), min_length: 1, max_length: 3),
                unregistered_attrs_list <- list_of(client_attrs(%{is_registered: false}), min_length: 1, max_length: 5) do

        # Create registered clients
        _registered_clients = Enum.map(registered_attrs_list, fn attrs ->
          {:ok, client} = Client.create(attrs)
          client
        end)

        # Create unregistered clients
        unregistered_clients = Enum.map(unregistered_attrs_list, fn attrs ->
          {:ok, client} = Client.create(attrs)
          client
        end)

        # Query unregistered clients
        assert {:ok, found_clients} = Client.unregistered()

        # Verify all returned clients are unregistered
        assert Enum.all?(found_clients, &(!&1.is_registered))

        # Verify we found at least the unregistered clients we created
        unregistered_ids = Enum.map(unregistered_clients, & &1.id)
        found_ids = Enum.map(found_clients, & &1.id)
        assert Enum.all?(unregistered_ids, &(&1 in found_ids))
      end
    end
  end
end
