alias RivaAshWeb.Live.Hooks, as: Hooks
alias Phoenix.Component, as: Component

defmodule RivaAshWeb.Live.Hooks.LocaleHook do
  @moduledoc """
  LiveView on_mount hook to ensure the Gettext locale is set for LiveViews.

  Resolution order (mirrors the Locale plug where applicable):
    1. params["locale"]
    2. session["locale"]
    3. socket.assigns.current_user locale
    4. default "en"

  Note:
  - Accept-Language is not available during websocket mounts; rely on session/assigns.
  - This sets the process-wide Gettext locale for the connected LiveView process.
  """
  import Phoenix.LiveView
  alias RivaAshWeb.Gettext

  @doc """
  Register in LiveViews via:

      on_mount RivaAshWeb.Live.Hooks.LocaleHook

  or with a named hook like:

      on_mount {RivaAshWeb.Live.Hooks.LocaleHook, :default}
  """
  def on_mount(_hook, params, session, socket) do
    locale =
      get_param_locale(params) ||
        get_session_locale(session) ||
        get_user_locale(socket) ||
        Gettext.default_locale()

    Gettext.put_locale(locale)

    {:cont,
     socket
     |> Phoenix.Component.assign_new(:locale, fn -> locale end)}
  end

  defp get_param_locale(%{"locale" => l}) when is_binary(l) and l != "" do
    normalize_and_validate(l)
  end

  defp get_unmatchedparam_unmatchedlocale(_unmatched), do: nil

  defp get_session_locale(%{"locale" => l}) when is_binary(l) and l != "" do
    normalize_and_validate(l)
  end

  defp get_unmatchedsession_unmatchedlocale(_unmatched), do: nil

  defp get_user_locale(%{assigns: %{current_user: user}}) when is_map(user) do
    case Map.get(user, :locale) || Map.get(user, "locale") do
      l when is_binary(l) and l != "" -> normalize_and_validate(l)
      _unmatchedunmatched -> nil
    end
  end

  defp get_unmatcheduser_unmatchedlocale(_unmatched), do: nil

  defp normalize_and_validate(locale) do
    norm = normalize(locale)
    if supported?(norm), do: norm, else: nil
  end

  defp normalize(locale) do
    locale
    |> String.downcase()
    |> String.replace("_unmatched", "-")
    |> String.split("-", parts: 2)
    |> List.first()
  end

  defp supported?(locale) do
    locale in Gettext.supported_locales()
  end
end
