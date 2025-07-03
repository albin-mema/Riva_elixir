defmodule RivaAshWeb.API.V1.ItemsControllerTest do
  use RivaAshWeb.EndpointCase, async: true
  import RivaAsh.TestHelpers

  alias RivaAsh.Resources.Item

  # JSON:API formatted test data for HTTP requests
  @create_attrs %{
    "data" => %{
      "type" => "item",
      "attributes" => %{
        "name" => "Test Item"
      }
    }
  }

  @update_attrs %{
    "data" => %{
      "type" => "item",
      "attributes" => %{
        "name" => "Updated Item"
      }
    }
  }

  @invalid_attrs %{
    "data" => %{
      "type" => "item",
      "attributes" => %{
        "name" => nil
      }
    }
  }

  describe "GET /api/items" do
    test "lists all items", %{conn: conn} do
      # Create test items
      item1 = create_item!(%{name: "Item 1"})
      item2 = create_item!(%{name: "Item 2"})

      # Make the request
      conn = get(conn, "/api/items")

      # Verify response
      assert %{
               "data" => [
                 %{"id" => id1, "type" => "item", "attributes" => %{"name" => name1}},
                 %{"id" => id2, "type" => "item", "attributes" => %{"name" => name2}}
               ]
             } = json_response(conn, 200)

      # Verify all items are present
      assert {item1.id, item2.id} == {id1, id2} || {item1.id, item2.id} == {id2, id1}
      assert "Item 1" in [name1, name2]
      assert "Item 2" in [name1, name2]
    end

    test "returns empty list when no items exist", %{conn: conn} do
      # Ensure no items exist
      Item.read!()
      |> Enum.each(&Item.destroy!/1)

      conn = get(conn, "/api/items")
      assert json_response(conn, 200) == %{"data" => []}
    end

    test "filters items by name", %{conn: conn} do
      create_item!(%{name: "Special Item"})
      create_item!(%{name: "Other Item"})

      conn = get(conn, "/api/items?filter[name]=Special")

      assert %{
               "data" => [
                 %{"attributes" => %{"name" => "Special Item"}}
               ]
             } = json_response(conn, 200)
    end
  end

  describe "GET /api/items/:id" do
    test "shows a single item", %{conn: conn} do
      # Create a test item
      item = create_item!(%{name: "Test Item"})

      # Make the request
      conn = get(conn, "/api/items/#{item.id}")

      # Verify response
      assert %{
               "data" => %{
                 "id" => item_id,
                 "type" => "item",
                 "attributes" => %{"name" => "Test Item"}
               }
             } = json_response(conn, 200)

      # Verify the ID matches
      assert item_id == item.id
    end

    test "returns 404 when item does not exist", %{conn: conn} do
      # Make the request with a non-existent ID
      non_existent_id = "00000000-0000-0000-0000-000000000000"

      conn = get(conn, "/api/items/#{non_existent_id}")

      # Verify error response
      assert %{"errors" => [%{"status" => "404"}]} = json_response(conn, 404)
    end
  end

  describe "POST /api/items" do
    test "creates an item", %{conn: conn} do
      # Make the request
      conn = post(conn, "/api/items", @create_attrs)

      # Verify response
      assert %{
               "data" => %{
                 "id" => id,
                 "type" => "item",
                 "attributes" => %{"name" => "Test Item"}
               }
             } = json_response(conn, 201)

      # Verify the item was created in the database
      assert {:ok, _item} = Item.by_id(id)
    end

    test "returns error with invalid data", %{conn: conn} do
      conn = post(conn, "/api/items", @invalid_attrs)

      assert %{
               "errors" => [
                 %{
                   "detail" => "is required",
                   "source" => %{"pointer" => "/data/attributes/name"},
                   "status" => "422"
                 }
               ]
             } = json_response(conn, 422)
    end
  end

  describe "PATCH /api/items/:id" do
    test "updates an item", %{conn: conn} do
      item = create_item!(%{name: "Original Name"})

      conn = patch(conn, "/api/items/#{item.id}", @update_attrs)

      assert %{
               "data" => %{
                 "id" => item_id,
                 "attributes" => %{"name" => "Updated Item"}
               }
             } = json_response(conn, 200)

      # Verify the ID matches
      assert item_id == item.id
    end

    test "returns error for invalid update", %{conn: conn} do
      item = create_item!(%{name: "Original Name"})

      conn = patch(conn, "/api/items/#{item.id}", @invalid_attrs)

      assert %{"errors" => [%{"status" => "422"}]} = json_response(conn, 422)
    end
  end

  describe "DELETE /api/items/:id" do
    test "deletes an item", %{conn: conn} do
      item = create_item!(%{name: "To be deleted"})

      conn = delete(conn, "/api/items/#{item.id}")
      assert response(conn, 204)

      # Verify item was deleted
      assert {:error, _} = Item.by_id(item.id)
    end
  end

  test "can create items with various valid names", %{conn: conn} do
    names = [
      "Test Item",
      "Item 123",
      "Test-Item_123",
      "Test Item with spaces",
      "Test Item with numbers 123"
    ]

    for name <- names do
      response =
        conn
        |> post("/api/items", %{
          "data" => %{
            "type" => "item",
            "attributes" => %{"name" => name}
          }
        })
        |> json_response(201)

      assert %{"data" => %{"id" => id}} = response
      assert {:ok, _} = Item.by_id(id)
    end
  end
end
