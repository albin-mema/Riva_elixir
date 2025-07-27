defmodule RivaAsh.Resources.PublicSearchTest do
  use RivaAsh.DataCase, async: true

  alias RivaAsh.Resources.{Business, Item, User, Section, Plot}

  describe "Business public search" do
    setup do
      # Create a test user
      {:ok, user} = User
      |> Ash.Changeset.for_create(:create, %{
        email: "test@example.com",
        password: "password123",
        role: :user
      })
      |> Ash.create(domain: RivaAsh.Accounts)

      # Create public businesses
      {:ok, public_business1} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Coffee Paradise",
        description: "Internal description",
        public_description: "Best coffee in town",
        is_public_searchable: true,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, public_business2} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Tea House",
        description: "Great tea selection",
        is_public_searchable: true,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      # Create private business
      {:ok, private_business} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Private Club",
        description: "Members only",
        is_public_searchable: false,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      %{
        user: user,
        public_business1: public_business1,
        public_business2: public_business2,
        private_business: private_business
      }
    end

    test "public_search returns only public businesses", %{public_business1: pb1, public_business2: pb2, private_business: priv} do
      {:ok, results} = Business
      |> Ash.Query.for_read(:public_search)
      |> Ash.read(domain: RivaAsh.Domain)

      business_ids = Enum.map(results, & &1.id)
      
      assert pb1.id in business_ids
      assert pb2.id in business_ids
      refute priv.id in business_ids
    end

    test "public_search filters by search term in name", %{public_business1: pb1, public_business2: pb2} do
      {:ok, results} = Business
      |> Ash.Query.for_read(:public_search, %{search_term: "coffee"})
      |> Ash.read(domain: RivaAsh.Domain)

      business_ids = Enum.map(results, & &1.id)
      
      assert pb1.id in business_ids
      refute pb2.id in business_ids
    end

    test "public_search filters by search term in public description", %{public_business1: pb1, public_business2: pb2} do
      {:ok, results} = Business
      |> Ash.Query.for_read(:public_search, %{search_term: "best"})
      |> Ash.read(domain: RivaAsh.Domain)

      business_ids = Enum.map(results, & &1.id)
      
      assert pb1.id in business_ids
      refute pb2.id in business_ids
    end

    test "public_search works without authentication" do
      # This should not require an actor
      {:ok, results} = Business
      |> Ash.Query.for_read(:public_search)
      |> Ash.read(domain: RivaAsh.Domain)

      assert is_list(results)
    end
  end

  describe "Item public search" do
    setup do
      # Create test user and business
      {:ok, user} = User
      |> Ash.Changeset.for_create(:create, %{
        email: "test@example.com", 
        password: "password123",
        role: :user
      })
      |> Ash.create(domain: RivaAsh.Accounts)

      {:ok, public_business} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Public Cafe",
        is_public_searchable: true,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, private_business} = Business
      |> Ash.Changeset.for_create(:create, %{
        name: "Private Restaurant",
        is_public_searchable: false,
        owner_id: user.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      # Create plots and sections (required for items)
      {:ok, public_plot} = Plot
      |> Ash.Changeset.for_create(:create, %{
        name: "Main Area",
        business_id: public_business.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, private_plot} = Plot
      |> Ash.Changeset.for_create(:create, %{
        name: "Private Area", 
        business_id: private_business.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, public_section} = Section
      |> Ash.Changeset.for_create(:create, %{
        name: "Tables",
        plot_id: public_plot.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, private_section} = Section
      |> Ash.Changeset.for_create(:create, %{
        name: "VIP Tables",
        plot_id: private_plot.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      # Create items
      {:ok, public_item} = Item
      |> Ash.Changeset.for_create(:create, %{
        name: "Table 1",
        description: "Nice table",
        public_description: "Perfect table for groups",
        is_public_searchable: true,
        is_active: true,
        business_id: public_business.id,
        section_id: public_section.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      {:ok, private_item} = Item
      |> Ash.Changeset.for_create(:create, %{
        name: "VIP Table",
        description: "Exclusive table",
        is_public_searchable: false,
        is_active: true,
        business_id: private_business.id,
        section_id: private_section.id
      })
      |> Ash.create(domain: RivaAsh.Domain)

      %{
        user: user,
        public_business: public_business,
        private_business: private_business,
        public_item: public_item,
        private_item: private_item
      }
    end

    test "public_search returns only public items from public businesses", %{public_item: pub_item, private_item: priv_item} do
      {:ok, results} = Item
      |> Ash.Query.for_read(:public_search)
      |> Ash.read(domain: RivaAsh.Domain)

      item_ids = Enum.map(results, & &1.id)
      
      assert pub_item.id in item_ids
      refute priv_item.id in item_ids
    end

    test "public_search filters by search term", %{public_item: pub_item} do
      {:ok, results} = Item
      |> Ash.Query.for_read(:public_search, %{search_term: "perfect"})
      |> Ash.read(domain: RivaAsh.Domain)

      item_ids = Enum.map(results, & &1.id)
      assert pub_item.id in item_ids
    end

    test "public_search works without authentication" do
      # This should not require an actor
      {:ok, results} = Item
      |> Ash.Query.for_read(:public_search)
      |> Ash.read(domain: RivaAsh.Domain)

      assert is_list(results)
    end
  end
end
