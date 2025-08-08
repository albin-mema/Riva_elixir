alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.HTML.FormField, as: FormField
alias Phoenix.LiveView.Rendered, as: Rendered
alias Phoenix.HTML, as: HTML

defmodule RivaAshWeb.Components.Atoms.FileUpload do
  import RivaAshWeb.Gettext, only: [dgettext: 2]

  @moduledoc """
  File upload component with validation, progress tracking, and accessibility.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          optional(:field) => Phoenix.HTML.FormField.t(),
          optional(:value) => String.t() | list(),
          optional(:accept) => list(String.t()),
          optional(:multiple) => boolean(),
          optional(:max_files) => integer(),
          optional(:max_file_size) => integer(),
          optional(:disabled) => boolean(),
          optional(:required) => boolean(),
          optional(:drag_drop) => boolean(),
          optional(:show_preview) => boolean(),
          optional(:size) => String.t(),
          optional(:variant) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a file upload input with validation and progress tracking.
  """
  @spec file_upload(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:field, Phoenix.HTML.FormField, default: nil)
  attr(:value, :any, default: nil)
  attr(:accept, :list, default: [])
  attr(:multiple, :boolean, default: false)
  attr(:max_files, :integer, default: 1)
  # 10MB default
  attr(:max_file_size, :integer, default: 10_485_760)
  attr(:disabled, :boolean, default: false)
  attr(:required, :boolean, default: false)
  attr(:drag_drop, :boolean, default: true)
  attr(:show_preview, :boolean, default: true)
  attr(:size, :string, default: "md", values: ~w(sm md lg))
  attr(:variant, :string, default: "default", values: ~w(default error success))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  @impl true
  def file_upload(assigns) do
    assigns
    |> build_file_upload_attrs()
    |> render_file_upload()
  end

  @spec build_file_upload_attrs(assigns :: assigns()) :: assigns()
  defp build_file_upload_attrs(assigns) do
    default_max_size = Application.get_env(:riva_ash, :file_upload_max_size, 10_485_760)

    default_accept =
      Application.get_env(:riva_ash, :file_upload_accept, [
        "image/jpeg",
        "image/png",
        "image/gif",
        "application/pdf",
        "text/plain"
      ])

    assigns
    |> Map.put_new(:max_file_size, default_max_size)
    |> Map.put_new(:accept, default_accept)
    |> validate_file_upload_attrs()
  end

  @spec validate_file_upload_attrs(assigns :: assigns()) :: assigns()
  defp validate_file_upload_attrs(assigns) do
    with :ok <- validate_max_files(assigns[:max_files]),
         :ok <- validate_max_file_size(assigns[:max_file_size]),
         :ok <- validate_accept_types(assigns[:accept]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid file upload attributes: #{reason}"
    end
  end

  @spec validate_max_files(integer()) :: :ok | {:error, String.t()}
  defp validate_max_files(max_files) when max_files > 0, do: :ok
  defp validate_max_files(_), do: {:error, "max_files must be greater than 0"}

  @spec validate_max_file_size(integer()) :: :ok | {:error, String.t()}
  defp validate_max_file_size(size) when size > 0, do: :ok
  defp validate_max_file_size(_), do: {:error, "max_file_size must be greater than 0"}

  @spec validate_accept_types(list(String.t())) :: :ok | {:error, String.t()}
  defp validate_accept_types(accept_types) do
    Enum.reduce_while(accept_types, :ok, fn type, :ok ->
      case validate_mime_type(type) do
        :ok -> {:cont, :ok}
        {:error, _} -> {:halt, {:error, "Invalid MIME type in accept list"}}
      end
    end)
  end

  @spec validate_mime_type(String.t()) :: :ok | {:error, String.t()}
  defp validate_mime_type(type) do
    valid_types = [
      "image/jpeg",
      "image/png",
      "image/gif",
      "image/webp",
      "application/pdf",
      "text/plain",
      "text/csv",
      "application/vnd.ms-excel",
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      "application/msword",
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    ]

    if type in valid_types do
      :ok
    else
      {:error, "Unsupported MIME type: #{type}"}
    end
  end

  @spec render_file_upload(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_file_upload(assigns) do
    ~H"""
    <div class="file-upload-container #{@class}">
      <div
        id={@id || "file-upload-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
        class={build_drop_zone_class(@size, @variant, @disabled)}
        data-multiple={@multiple}
        data-max-files={@max_files}
        data-max-file-size={@max_file_size}
        data-accept={@accept |> Enum.join(",")}
        phx-hook="FileUpload"
        {@rest}
      >
        <input
          type="file"
          name={@name || @field.name}
          id={"file-input-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"}
          accept={@accept |> Enum.join(",")}
          multiple={@multiple}
          disabled={@disabled}
          required={@required}
          class="hidden"
        />

        <div class="file-upload-content">
          <.icon name={:upload} class="mx-auto h-12 w-12 text-gray-400" />
          <div class="text-center">
            <p class="text-sm text-gray-600">
              <span class="font-medium text-blue-600 hover:text-blue-500">
                <%= dgettext("ui", "Click to upload") %>
              </span>
              <span><%= dgettext("ui", " or drag and drop") %></span>
            </p>
            <p class="text-xs text-gray-500 mt-1">
              <%= build_file_size_text(@max_file_size) %>
              <%= if @multiple, " • Up to #{@max_files} files", "" %>
            </p>
          </div>
        </div>
      </div>

      <%= if @show_preview and assigns[:value] do %>
        <div class="file-preview mt-4">
          <%= render_file_preview(@value, @accept) %>
        </div>
      <% end %>

      <%= if @field && @field.errors != [] do %>
        <div class="error-message mt-2 text-sm text-red-600">
          <%= for error <- @field.errors do %>
            <%= error %>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  @spec build_drop_zone_class(String.t(), String.t(), boolean()) :: String.t()
  defp build_drop_zone_class(size, variant, disabled) do
    base_classes = "relative border-2 border-dashed rounded-lg cursor-pointer transition-colors duration-200"

    size_classes =
      case size do
        "sm" -> "p-4"
        "lg" -> "p-6"
        _ -> "p-5"
      end

    variant_classes =
      case variant do
        "error" -> "border-red-300 bg-red-50 hover:border-red-400"
        "success" -> "border-green-300 bg-green-50 hover:border-green-400"
        _ -> "border-gray-300 bg-gray-50 hover:border-gray-400"
      end

    disabled_classes = if(disabled, "opacity-50 cursor-not-allowed border-gray-200", "")

    "#{base_classes} #{size_classes} #{variant_classes} #{disabled_classes}"
  end

  @spec build_file_size_text(integer()) :: String.t()
  defp build_file_size_text(max_size) do
    cond do
      max_size >= 100_000_000 ->
        "#{div(max_size, 100_000_000)}" <> dgettext("dates_numbers", "GB max")

      max_size >= 1_000_000 ->
        "#{div(max_size, 1_000_000)}" <> dgettext("dates_numbers", "MB max")

      max_size >= 1_000 ->
        "#{div(max_size, 1_000)}" <> dgettext("dates_numbers", "KB max")

      true ->
        "#{max_size}" <> dgettext("dates_numbers", "B max")
    end
  end

  @spec render_file_preview(String.t() | list(), list(String.t())) :: Phoenix.LiveView.Rendered.t()
  defp render_file_preview(value, _accept) when is_binary(value) do
    ~H"""
    <div class="file-preview-item">
      <div class="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
        <div class="flex items-center space-x-3">
          <.icon name={:document} class="h-5 w-5 text-gray-400" />
          <span class="text-sm font-medium text-gray-900 truncate">
            <%= String.slice(value, 0..30) <> if String.length(value) > 30, "...", "" %>
          </span>
        </div>
        <button type="button" class="text-red-600 hover:text-red-800">
          <.icon name={:x} class="h-4 w-4" />
        </button>
      </div>
    </div>
    """
  end

  defp render_file_preview(values, accept) when is_list(values) do
    ~H"""
    <div class="space-y-2">
      <%= for value <- values do %>
        <%= render_file_preview(value, accept) %>
      <% end %>
    </div>
    """
  end

  defp render_file_preview(_, _), do: ""
end
