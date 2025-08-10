defmodule RivaAsh.PropertyTesting.NavigationGraph do
  @moduledoc """
  Build a navigation graph by crawling pages and extracting navigable targets.

  This is a lightweight helper for property tests that want to "click" through
  links without requiring a full browser. It uses the test conn to fetch pages,
  parses HTML (via Floki if available), and follows anchor/LiveView navigation
  attributes like href/patch/navigate.
  """

  import Phoenix.ConnTest

  @type path :: String.t()
  @type status :: non_neg_integer()

  @doc """
  Perform a bounded random walk by following links starting from start_path.

  Returns a map with :visited_paths (ordered list), :visited_edges, and
  :statuses (list of {path, status}).
  """
  def random_walk(conn, start_path, steps, opts \\ []) when is_integer(steps) and steps >= 0 do
    allow_redirects = Keyword.get(opts, :allow_redirects, true)
    endpoint = Keyword.fetch!(opts, :endpoint)

    do_walk(
      conn,
      start_path,
      steps,
      %{
        visited_paths: [],
        visited_edges: [],
        statuses: []
      },
      allow_redirects,
      endpoint
    )
  end

  defp do_walk(_conn, _path, 0, acc, _redir, _endpoint), do: finalize(acc)

  defp do_walk(conn, path, remaining, acc, allow_redirects, endpoint) do
    {conn2, effective_path, status, html} = fetch(conn, path, allow_redirects: allow_redirects, endpoint: endpoint)

    links =
      extract_links(html)
      |> Enum.map(&normalize_path/1)
      |> Enum.filter(&navigable_path?/1)

    next_path = pick_next(links)

    acc2 = %{
      visited_paths: acc.visited_paths ++ [effective_path],
      visited_edges: acc.visited_edges ++ if(next_path, do: [{effective_path, next_path}], else: []),
      statuses: acc.statuses ++ [{effective_path, status}]
    }

    case next_path do
      nil -> finalize(acc2)
      _ -> do_walk(conn2, next_path, remaining - 1, acc2, allow_redirects, endpoint)
    end
  end

  defp finalize(acc) do
    Map.update!(acc, :visited_paths, &Enum.uniq/1)
  end

  @doc """
  Fetch a page and optionally follow redirects. Returns {conn, effective_path, status, html}.
  """
  def fetch(conn, path, opts \\ []) do
    allow_redirects = Keyword.get(opts, :allow_redirects, true)
    endpoint = Keyword.fetch!(opts, :endpoint)

    conn1 = Phoenix.ConnTest.dispatch(conn, endpoint, :get, path, %{})
    status = conn1.status || 0

    if allow_redirects and status in [301, 302, 303, 307, 308] do
      location = Plug.Conn.get_resp_header(conn1, "location") |> List.first()

      case normalize_path(location) do
        nil -> {conn1, path, status, conn1.resp_body || ""}
        next -> fetch(conn1, next, Keyword.put(opts, :allow_redirects, allow_redirects))
      end
    else
      {conn1, path, status, conn1.resp_body || ""}
    end
  end

  @doc """
  Extract navigable targets from HTML: a[href], elements with [patch] or [navigate].
  Falls back to a basic regex scan if Floki is unavailable.
  """
  def extract_links(html) when is_binary(html) do
    cond do
      Code.ensure_loaded?(Floki) ->
        links =
          Floki.find(html, ~s(a[href]))
          |> Enum.map(&Floki.attribute(&1, "href"))
          |> List.flatten()

        patches =
          Floki.find(html, ~s([patch]))
          |> Enum.map(&Floki.attribute(&1, "patch"))
          |> List.flatten()

        navigates =
          Floki.find(html, ~s([navigate]))
          |> Enum.map(&Floki.attribute(&1, "navigate"))
          |> List.flatten()

        Enum.uniq(links ++ patches ++ navigates)

      true ->
        # Fallback: naive scans
        hrefs = Regex.scan(~r/href="([^"]+)"/, html) |> Enum.map(fn [_, p] -> p end)
        patches = Regex.scan(~r/patch="([^"]+)"/, html) |> Enum.map(fn [_, p] -> p end)
        navigates = Regex.scan(~r/navigate="([^"]+)"/, html) |> Enum.map(fn [_, p] -> p end)
        Enum.uniq(hrefs ++ patches ++ navigates)
    end
  end

  @doc """
  Normalize a URL into a local path. Returns nil if not navigable.
  """
  def normalize_path(nil), do: nil

  def normalize_path(url) when is_binary(url) do
    cond do
      url == "" ->
        nil

      String.starts_with?(url, "#") ->
        nil

      String.starts_with?(url, "mailto:") ->
        nil

      String.starts_with?(url, "tel:") ->
        nil

      String.starts_with?(url, "http://") or String.starts_with?(url, "https://") ->
        case URI.parse(url) do
          %URI{path: path} when is_binary(path) -> path
          _ -> nil
        end

      String.starts_with?(url, "/") ->
        url

      true ->
        nil
    end
  end

  @doc """
  Decide if a path is navigable for our purposes.
  """
  def navigable_path?(path) when is_binary(path) do
    not String.contains?(path, "*") and
      not String.ends_with?(path, ".json") and
      not String.ends_with?(path, ".xml") and
      not String.ends_with?(path, ".csv") and
      not String.starts_with?(path, "/api") and
      not String.contains?(path, "/auth/") and
      not String.contains?(path, "sign-out")
  end

  def navigable_path?(_), do: false

  defp pick_next([]), do: nil

  defp pick_next(links) do
    idx = :rand.uniform(length(links)) - 1
    Enum.at(links, idx)
  end
end
