defmodule RivaAshWeb.Components.Atoms.Modal do
  @moduledoc """
  Modal component with accessibility, keyboard navigation, and animations.
  """
  use Phoenix.Component
  import RivaAshWeb.CoreComponents

  @type assigns :: %{
          optional(:id) => String.t(),
          optional(:title) => String.t(),
          optional(:open) => boolean(),
          optional(:on_close) => String.t(),
          optional(:size) => String.t(),
          optional(:variant) => String.t(),
          optional(:class) => String.t(),
          optional(:rest) => map(),
          optional(:inner_block) => any()
        }

  @doc """
  Renders a modal dialog with accessibility features and animations.
  """
  @spec modal(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:id, :string, default: nil)
  attr(:title, :string, default: nil)
  attr(:open, :boolean, default: false)
  attr(:on_close, :string, default: "close_modal")
  attr(:size, :string, default: "md", values: ~w(sm md lg xl full))
  attr(:variant, :string, default: "default", values: ~w(default danger success warning))
  attr(:class, :string, default: "")
  attr(:rest, :global)

  slot(:inner_block, required: true)

  @impl true
  def modal(assigns) do
    assigns
    |> build_modal_attrs()
    |> render_modal()
  end

  @spec build_modal_attrs(assigns :: assigns()) :: assigns()
  defp build_modal_attrs(assigns) do
    default_size = Application.get_env(:riva_ash, :modal_default_size, "md")

    assigns
    |> Map.put_new(:size, default_size)
    |> validate_modal_attrs()
  end

  @spec validate_modal_attrs(assigns :: assigns()) :: assigns()
  defp validate_modal_attrs(assigns) do
    with :ok <- validate_size(assigns[:size]),
         :ok <- validate_variant(assigns[:variant]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid modal attributes: #{reason}"
    end
  end

  @spec validate_size(String.t()) :: :ok | {:error, String.t()}
  defp validate_size("sm"), do: :ok
  defp validate_size("md"), do: :ok
  defp validate_size("lg"), do: :ok
  defp validate_size("xl"), do: :ok
  defp validate_size("full"), do: :ok
  defp validate_size(_), do: {:error, "Size must be one of: sm, md, lg, xl, full"}

  @spec validate_variant(String.t()) :: :ok | {:error, String.t()}
  defp validate_variant("default"), do: :ok
  defp validate_variant("danger"), do: :ok
  defp validate_variant("success"), do: :ok
  defp validate_variant("warning"), do: :ok
  defp validate_variant(_), do: {:error, "Variant must be one of: default, danger, success, warning"}

  @spec render_modal(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_modal(assigns) do
    modal_id = assigns[:id] || "modal-#{:crypto.strong_rand_bytes(8) |> Base.encode16()}"

    ~H"""
    <div
      id={modal_id}
      class="modal-container #{@class}"
      role="dialog"
      aria-modal="true"
      aria-labelledby={"modal-title-#{modal_id}"}
      aria-describedby={"modal-description-#{modal_id}"}
      data-open={@open}
      {@rest}
    >
      <!-- Backdrop -->
      <div
        class="modal-backdrop fixed inset-0 bg-black bg-opacity-50 transition-opacity duration-300"
        phx-click={@on_close}
        aria-hidden="true"
      />

      <!-- Modal Content -->
      <div
        class="modal-content fixed inset-0 z-50 overflow-y-auto"
        role="document"
      >
        <div class="flex min-h-screen items-center justify-center p-4">
          <div
            class={build_modal_class(@size, @variant)}
            role="dialog"
            aria-modal="true"
            tabindex="0"
          >
            <!-- Modal Header -->
            <%= if @title do %>
              <div class="modal-header">
                <h3 id={"modal-title-#{modal_id}"} class="modal-title">
                  <%= @title %>
                </h3>
                <button
                  type="button"
                  class="modal-close-button"
                  phx-click={@on_close}
                  aria-label="Close modal"
                  aria-expanded="false"
                >
                  <.icon name={:x} class="h-6 w-6" />
                </button>
              </div>
            <% end %>

            <!-- Modal Body -->
            <div id={"modal-description-#{modal_id}"} class="modal-body">
              <%= render_slot(@inner_block) %>
            </div>

            <!-- Modal Footer -->
            <div class="modal-footer">
              <button
                type="button"
                class="modal-cancel-button"
                phx-click={@on_close}
              >
                Cancel
              </button>
              <%= render_modal_footer_content(assigns) %>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @spec build_modal_class(String.t(), String.t()) :: String.t()
  defp build_modal_class(size, variant) do
    base_classes = "relative bg-white rounded-lg shadow-xl transform transition-all duration-300"

    size_classes =
      case size do
        "sm" -> "w-96"
        "md" -> "w-[500px]"
        "lg" -> "w-[600px]"
        "xl" -> "w-[800px]"
        "full" -> "w-full max-w-4xl mx-4"
      end

    variant_classes =
      case variant do
        "danger" -> "border-t-4 border-red-500"
        "success" -> "border-t-4 border-green-500"
        "warning" -> "border-t-4 border-yellow-500"
        _ -> "border-t-4 border-blue-500"
      end

    "#{base_classes} #{size_classes} #{variant_classes}"
  end

  @spec render_modal_footer_content(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_modal_footer_content(assigns) do
    ~H"""
    <button
      type="button"
      class={build_modal_action_class(@variant)}
      phx-click={@on_close}
    >
      <%= get_modal_action_text(@variant) %>
    </button>
    """
  end

  @spec build_modal_action_class(String.t()) :: String.t()
  defp build_modal_action_class("danger") do
    "modal-action-button bg-red-600 text-white hover:bg-red-700 focus:ring-red-500"
  end

  defp build_modal_action_class("success") do
    "modal-action-button bg-green-600 text-white hover:bg-green-700 focus:ring-green-500"
  end

  defp build_modal_action_class("warning") do
    "modal-action-button bg-yellow-600 text-white hover:bg-yellow-700 focus:ring-yellow-500"
  end

  defp build_modal_action_class(_) do
    "modal-action-button bg-blue-600 text-white hover:bg-blue-700 focus:ring-blue-500"
  end

  @spec get_modal_action_text(String.t()) :: String.t()
  defp get_modal_action_text("danger"), do: "Delete"
  defp get_modal_action_text("success"), do: "Save"
  defp get_modal_action_text("warning"), do: "Confirm"
  defp get_modal_action_text(_), do: "Confirm"
end
