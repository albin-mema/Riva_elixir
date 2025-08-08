defmodule RivaAshWeb.Gettext do
  @moduledoc """
  Gettext backend for RivaAshWeb.

  Provides the standard gettext macros and minimal helpers. Keep this module
  lightweight; put locale resolution logic in plugs/hooks.

  Domains in use (per ADR): default, ui, errors, emails, dates_numbers, auth, navigation.
  """

  use Gettext, otp_app: :riva_ash

  @type locale :: String.t()

  @spec default_locale() :: locale()
  def default_locale, do: Application.get_env(:riva_ash, :default_locale, "en")

  @spec supported_locales() :: [locale()]
  def supported_locales, do: Application.get_env(:riva_ash, :supported_locales, ["en"])

  @spec get_locale() :: locale()
  def get_locale, do: Gettext.get_locale(RivaAshWeb.Gettext)

  @spec put_locale(locale()) :: :ok
  def put_locale(locale) when is_binary(locale) do
    Gettext.put_locale(RivaAshWeb.Gettext, locale)
  end
end
