defmodule RivaAshWeb.Api.V1.JsonApiBasicTest do
  use RivaAshWeb.ConnCase, async: true

  import RivaAsh.Test.JsonApiHelpers
  import Phoenix.ConnTest

  @businesses "/api/v1/businesses"

  @spec test_list_returns_data_array_with_type_id_attributes :: :ok
  test "list returns data array with type/id/attributes", %{conn: conn} do
    conn = json_api_get(conn, @businesses)
    json = json_response(conn, 200)

    assert %{"data" => data} = json
    assert is_list(data)

    Enum.each(data, fn item ->
      assert Map.has_key?(item, "type")
      assert Map.has_key?(item, "id")
      assert Map.has_key?(item, "attributes")
    end)
  end

  @spec test_invalid_filter_returns_json_api_error_object :: :ok
  test "invalid filter returns JSON:API error object", %{conn: conn} do
    conn = json_api_get(conn, @businesses <> "?filter[unknown]=value")
    json = json_response(conn, 400)
    assert_error_shape(json)
  end

  @spec test_sparse_fieldsets_returns_only_requested_fields :: :ok
  test "sparse fieldsets returns only requested fields", %{conn: conn} do
    conn = json_api_get(conn, @businesses <> "?fields[business]=name,description")
    json = json_response(conn, 200)
    %{"data" => data} = json

    Enum.each(data, fn %{"attributes" => attrs} ->
      assert Map.has_key?(attrs, "name")
      assert Map.has_key?(attrs, "description")
    end)
  end
end