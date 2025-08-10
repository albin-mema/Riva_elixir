# alias RivaAshWeb.Plugs, as: Plugs

defmodule RivaAshWeb.Plugs.Locale do
  @moduledoc """
  Plug to resolve and set the current Gettext locale.

  Resolution order:
    1. "locale" param
    2. session
    3. current_user setting (if available in assigns/current_user, using :locale or "locale")
    4. Accept-Language header (first compatible language)
    5. default "en"

  When the "locale" param is present, it is persisted into the session.
  """
  import Plug.Conn
  alias RivaAshWeb.Gettext

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, _opts) do
    locale =
      get_param_locale(conn) ||
        get_session_locale(conn) ||
        get_user_locale(conn) ||
        get_accept_language_locale(conn) ||
        Gettext.default_locale()

    # Persist param locale to session (only when param present)
    conn =
      case get_param_locale(conn) do
        nil -> conn
        param_locale -> put_session(conn, "locale", param_locale)
      end

    Gettext.put_locale(locale)
    assign(conn, :locale, locale)
  end

  defp get_param_locale(%Plug.Conn{params: %{"locale" => l}}) when is_binary(l) and l != "" do
    normalize_and_validate(l)
  end

  # When there is no "locale" param, fall back to other resolution strategies
  defp get_param_locale(_conn), do: nil

  defp get_unmatchedparam_unmatchedlocale(_unmatched), do: nil

  defp get_session_locale(conn) do
    case get_session(conn, "locale") do
      l when is_binary(l) and l != "" -> normalize_and_validate(l)
      _unmatchedunmatched -> nil
    end
  end

  defp get_user_locale(%Plug.Conn{assigns: %{current_user: user}}) when is_map(user) do
    case Map.get(user, :locale) || Map.get(user, "locale") do
      l when is_binary(l) and l != "" -> normalize_and_validate(l)
      _ -> nil
    end
  end

  defp get_user_locale(_), do: nil

  defp get_unmatcheduser_unmatchedlocale(_unmatched), do: nil

  defp get_accept_language_locale(conn) do
    header = get_req_header(conn, "accept-language") |> List.first()

    with true <- is_binary(header),
         [candidate | _unmatched] <- parse_accept_language(header),
         candidate when is_binary(candidate) <- candidate,
         norm when is_binary(norm) <- normalize(candidate),
         true <- supported?(norm) do
      norm
    else
      _unmatchedunmatched -> nil
    end
  end

  defp parse_accept_language(header) when is_binary(header) do
    header
    |> String.split(",", trim: true)
    |> Enum.map(fn part ->
      part
      |> String.split(";", parts: 2)
      |> List.first()
      |> String.trim()
    end)
    |> Enum.reject(&(&1 == ""))
  end

  defp normalize_and_validate(locale) do
    norm = normalize(locale)
    if supported?(norm), do: norm, else: nil
  end

  defp normalize(locale) do
    locale
    |> String.downcase()
    |> String.replace("_unmatched", "-")
    |> String.split("-", parts: 2)
    |> Enum.at(0)
  end

  defp supported?(locale) do
    locale in Gettext.supported_locales()
  end
end
