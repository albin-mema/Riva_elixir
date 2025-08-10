defmodule RivaAsh.Components.UI.FileUpload do
  @moduledoc """
  File upload component with drag/drop functionality, progress tracking, and validation.
  Composed from atoms: Button, ProgressBar, and Skeleton components following atomic design principles.
  Implements ARIA roles for accessibility and integrates with design tokens for motion/spacing.
  """

  use Phoenix.Component
  import RivaAsh.Components.UI.Atoms

  @variants [:default, :error]
  @sizes [:sm, :md, :lg]
  @default_props %{
    variant: :default,
    size: :md,
    multiple: true,
    max_files: 5,
    max_size: 10_000_000, # 10MB
    allowed_types: ["image/*", "application/pdf"],
    aria_label: "File upload area"
  }

  def file_upload(assigns) do
    assigns = standard_assigns(assigns)

    ~H"""
    <div
      class={[
        "relative transition-all motion-smooth rounded-lg overflow-hidden",
        variant_classes(@variant),
        size_classes(@size),
        focus_styles(),
        @is_dragging && "border-primary bg-accent/10"
      ]}
      role="button"
      aria-label={@aria_label}
      aria-dropeffect="execute"
      aria-busy={@is_uploading}
      tabindex="0"
      phx-hook="FileUpload"
      id={"file-upload-#{@id}"}
      data-max-files={@max_files}
      data-max-size={@max_size}
      data-allowed-types={Jason.encode!(@allowed_types)}
    >
      <input
        type="file"
        hidden
        multiple={@multiple}
        id={"file-input-#{@id}"}
        phx-change="select_files"
        phx-trigger-action
        aria-hidden="true"
        tabindex="-1"
      />

      <div :if={not Enum.empty?(@files)} class="mb-4">
        <ul class="space-y-2">
          <%= for file <- @files do %>
            <li class="flex justify-between items-center bg-muted p-2 rounded">
              <div class="flex flex-1 items-center space-x-2 min-w-0">
                <RivaAsh.Components.UI.ProgressBar
                  value={file.progress}
                  size="sm"
                  class="flex-1 min-w-0"
                />
                <span class="text-sm truncate"><%= file.name %></span>
              </div>
              <RivaAsh.Components.UI.Button
                variant="ghost"
                size="sm"
                aria_label={"Remove #{file.name}"}
                phx-click="remove_file"
                phx-value-id={file.id}
                class="shrink-0"
              >
                ×
              </RivaAsh.Components.UI.Button>
            </li>
          <% end %>
        </ul>
      </div>

      <div :if={@is_dragging} class="py-8 text-center">
        <div class="mb-2 text-primary">
          <RivaAsh.Components.UI.Skeleton class="mx-auto w-24 h-8" :if={@is_uploading} />
          <span :if={not @is_uploading}>Drop files here</span>
        </div>
        <RivaAsh.Components.UI.Skeleton class="mx-auto w-48 h-4" :if={@is_uploading} />
      </div>

      <div :if={not @is_dragging and Enum.empty?(@files)} class="py-8 text-center">
        <p class="mb-2">Drag files here or</p>
        <RivaAsh.Components.UI.Button variant="primary" size="md" phx-click="trigger_file_input">
          Browse Files
        </RivaAsh.Components.UI.Button>
      </div>

      <div :if={@error} class="mt-2 px-2 text-destructive text-sm" role="alert" aria-live="assertive">
        <%= @error %>
      </div>
    </div>
    """
  end

  def standard_assigns(assigns, defaults \\ @default_props) do
    assigns
    |> assign_new(:variant, fn -> validate_variant(defaults[:variant]) end)
    |> assign_new(:size, fn -> validate_size(defaults[:size]) end)
    |> assign_new(:multiple, fn -> !!defaults[:multiple] end)
    |> assign_new(:max_files, fn -> defaults[:max_files] end)
    |> assign_new(:max_size, fn -> defaults[:max_size] end)
    |> assign_new(:allowed_types, fn -> defaults[:allowed_types] end)
    |> assign_new(:aria_label, fn -> defaults[:aria_label] end)
    |> assign_new(:files, fn -> [] end)
    |> assign_new(:is_dragging, fn -> false end)
    |> assign_new(:is_uploading, fn -> false end)
    |> assign_new(:error, fn -> nil end)
    |> assign_new(:id, fn -> "file-upload-#{System.unique_integer([:positive])}" end)
  end

  defp validate_variant(variant) when variant in @variants, do: variant
  defp validate_variant(_), do: :default

  defp validate_size(size) when size in @sizes, do: size
  defp validate_size(_), do: :md

  defp variant_classes(:default), do: "border-2 border-dashed border-border"
  defp variant_classes(:error), do: "border-2 border-destructive"

  defp size_classes(:sm), do: "p-2"
  defp size_classes(:md), do: "p-4"
  defp size_classes(:lg), do: "p-6"

  @doc """
  Handles file selection and validation
  """
  def handle_file_select(socket, files) do
    # Validate file count
    if length(socket.assigns.files) + length(files) > socket.assigns.max_files do
      socket
      |> assign(error: "Maximum #{socket.assigns.max_files} files allowed")
    else
      # Validate file sizes and types
      {valid_files, invalid_files} =
        Enum.split_with(files, fn file ->
          file.size <= socket.assigns.max_size &&
          Enum.any?(socket.assigns.allowed_types, &String.starts_with?(file.type, &1))
        end)

      # Process valid files
      new_files = Enum.map(valid_files, fn file ->
        %{id: System.unique_integer([:positive]), name: file.name, size: file.size, progress: 0}
      end)

      # Handle invalid files
      error_message =
        case {length(valid_files), length(invalid_files)} do
          {_, 0} -> nil
          {0, _} -> "No valid files selected"
          {_, _} -> "#{length(invalid_files)} file(s) invalid"
        end

      socket
      |> assign(files: socket.assigns.files ++ new_files)
      |> assign(error: error_message)
    end
  end

  @doc """
  Handles file removal
  """
  def handle_file_remove(socket, file_id) do
    updated_files = Enum.reject(socket.assigns.files, &(&1.id == file_id))
    assign(socket, files: updated_files)
  end

  @doc """
  Handles drag enter/over events
  """
  def handle_drag_start(socket) do
    assign(socket, is_dragging: true)
  end

  @doc """
  Handles drag leave/end events
  """
  def handle_drag_end(socket) do
    assign(socket, is_dragging: false)
  end

  @doc """
  Handles upload progress updates
  """
  def handle_upload_progress(socket, file_id, progress) do
    updated_files =
      Enum.map(socket.assigns.files, fn file ->
        if file.id == file_id, do: %{file | progress: progress}, else: file
      end)

    assign(socket, files: updated_files)
  end
end
