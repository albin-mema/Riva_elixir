defmodule RivaAshWeb.Gettext do
  @moduledoc """
  A module providing Internationalization with a gettext-based API.

  By using [Gettext](https://hexdocs.pm/gettext),
  your module gains a set of macros for translations, for example:

      import RivaAshWeb.Gettext

      # Simple translation
      gettext("Here is the string to translate")

      # Plural translation
      ngettext("Here is the string to translate",
               "Here are the strings to translate",
               3)

      # Domain-based translation
      dgettext("errors", "Here is the error message to translate")

  See the [Gettext Docs](https://hexdocs.pm/gettext) for detailed usage.

  This module provides enhanced internationalization support with
  locale management, fallback handling, and configuration-driven
  locale selection for the Riva Ash application.
  """

  @type gettext_backend :: atom()
  @type locale :: String.t()
  @type domain :: String.t()
  @type msgid :: String.t()
  @type msgid_plural :: String.t()
  @type count :: integer()
  @type result :: any()

  @doc """
  Returns the gettext backend identifier for the application.

  This is used by Gettext to identify the translation files
  and compiled message catalogs for this application.
  """
  @spec gettext_backend() :: gettext_backend()
  def gettext_backend, do: :riva_ash

  @doc """
  Returns the default locale for the application.

  Falls back to "en" (English) if no default locale is configured
  in the application environment.
  """
  @spec default_locale() :: locale()
  def default_locale, do: Application.get_env(:riva_ash, :default_locale, "en")

  @doc """
  Returns the list of supported locales for the application.

  Defaults to English ("en") and Albanian ("sq") if no specific
  locales are configured in the application environment.
  """
  @spec supported_locales() :: [locale()]
  def supported_locales, do: Application.get_env(:riva_ash, :supported_locales, ["en", "sq"])

  @doc """
  Retrieves the current active locale from the Gettext backend.

  Returns the locale that is currently set for translation operations.
  """
  @spec get_locale() :: locale()
  def get_locale do
    Gettext.get_locale(RivaAshWeb.Gettext)
  end

  @doc """
  Sets the current locale for translation operations.

  Updates the active locale in the Gettext backend. This affects
  all subsequent translation operations until the locale is changed.

  ## Parameters
    - `locale`: The locale code to set (e.g., "en", "sq")

  ## Returns
    `:ok` when the locale is successfully set
  """
  @spec put_locale(locale()) :: :ok
  def put_locale(locale) when is_binary(locale) do
    Gettext.put_locale(RivaAshWeb.Gettext, locale)
  end

  @doc """
  Executes a function with a temporarily set locale.

  Sets the specified locale, executes the provided function, and then
  restores the original locale. This is useful for localized operations
  without permanently changing the global locale state.

  ## Parameters
    - `locale`: The locale to use for the function execution
    - `func`: A zero-arity function to execute with the specified locale

  ## Returns
    The result of the function execution

  ## Example
      with_locale("sq", fn ->
        gettext("Welcome")  # Will be translated to Albanian
      end)
  """
  @spec with_locale(locale(), ( -> result)) :: result when result: var
  def with_locale(locale, func) when is_binary(locale) do
    original_locale = get_locale()
    put_locale(locale)
    result = func.()
    put_locale(original_locale)
    result
  end

  # Configure the Gettext backend with application-specific settings
  use Gettext.Backend,
    otp_app: :riva_ash,
    default_locale: Application.compile_env(:riva_ash, :default_locale, "en"),
    supported_locales: Application.compile_env(:riva_ash, :supported_locales, ["en", "sq"])
end
