defmodule RivaAshWeb.ResourceController do
  use Phoenix.Controller, formats: [:json]
  alias RivaAsh.Resources.Item
  alias RivaAsh.Domain

  action_fallback RivaAshWeb.FallbackController

  def index(conn, _params) do
    case Ash.read(Item, domain: Domain) do
      {:ok, items} ->
        conn
        |> put_status(:ok)
        |> json(%{data: items})

      {:error, error} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Failed to fetch items", details: error})
    end
  end

  def show(conn, %{"id" => id}) do
    case Ash.get(Item, id, domain: Domain) do
      {:ok, item} ->
        conn
        |> put_status(:ok)
        |> json(%{data: item})

      {:error, %Ash.Error.Query.NotFound{}} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Item not found"})

      {:error, error} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: "Failed to fetch item", details: error})
    end
  end

  def create(conn, %{"name" => name}) do
    case Ash.create(Item, %{name: name}, domain: Domain) do
      {:ok, item} ->
        conn
        |> put_status(:created)
        |> json(%{data: item})

      {:error, error} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to create item", details: format_error(error)})
    end
  end

  def create(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required parameter: name"})
  end

  def update(conn, %{"id" => id} = params) do
    with {:ok, item} <- Ash.get(Item, id, domain: Domain),
         {:ok, updated_item} <- Ash.update(item, Map.take(params, ["name"]), domain: Domain) do
      conn
      |> put_status(:ok)
      |> json(%{data: updated_item})
    else
      {:error, %Ash.Error.Query.NotFound{}} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Item not found"})

      {:error, error} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to update item", details: format_error(error)})
    end
  end

  def delete(conn, %{"id" => id}) do
    with {:ok, item} <- Ash.get(Item, id, domain: Domain),
         :ok <- Ash.destroy(item, domain: Domain) do
      conn
      |> put_status(:no_content)
      |> json(%{})
    else
      {:error, %Ash.Error.Query.NotFound{}} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: "Item not found"})

      {:error, error} ->
        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Failed to delete item", details: format_error(error)})
    end
  end

  defp format_error(%Ash.Error.Invalid{errors: errors}) do
    Enum.map(errors, fn error ->
      %{
        field: error.field,
        message: error.message
      }
    end)
  end

  defp format_error(error), do: inspect(error)
end