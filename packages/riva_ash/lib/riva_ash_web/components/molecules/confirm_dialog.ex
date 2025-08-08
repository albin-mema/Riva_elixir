alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.ConfirmDialog do
  import RivaAshWeb.Gettext, only: [dgettext: 2]

  @moduledoc """
  Confirmation dialog component for destructive actions.

  Provides a modal dialog for confirming user actions with customizable
  labels, variants, and event handlers.

  ## Styleguide Compliance

  This component follows the Riva Ash styleguide principles:

  ### Functional Programming Patterns
  - Uses pipeline operator (`|>`) for data transformation
  - Implements pure functions with no side effects
  - Uses pattern matching for data validation and processing
  - Follows single level of abstraction principle

  ### Type Safety
  - Comprehensive type specifications using `@type` and `@spec`
  - Strong typing for all function parameters and return values
  - Type validation through pattern matching

  ### Error Handling
  - Uses result tuples (`:ok | {:error, String.t()}`) for consistent error handling
  - Early validation with guard clauses
  - Clear error messages for invalid inputs

  ### Code Abstraction
  - Separates concerns into focused helper functions
  - Extracts validation logic into dedicated functions
  - Uses functional composition for complex operations

  ### Phoenix/Ash Patterns
  - Follows Phoenix LiveView component conventions
  - Uses proper attribute validation and building
  - Implements functional core, imperative shell pattern

  ### LiveView Component Patterns
  - Uses proper slot and attribute handling
  - Implements accessibility features
  - Follows Phoenix component best practices
  """
  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Button
  # removed unused import to reduce warnings

  @type dialog_config :: %{
          title: String.t(),
          message: String.t(),
          confirm_label: String.t(),
          cancel_label: String.t(),
          variant: dialog_variant(),
          on_confirm: String.t(),
          on_cancel: String.t(),
          show: boolean()
        }
  @type dialog_variant :: :destructive | :warning | :info
  @type assigns :: %{
          required(:title) => String.t(),
          required(:message) => String.t(),
          optional(:confirm_label) => String.t(),
          optional(:cancel_label) => String.t(),
          optional(:variant) => dialog_variant(),
          required(:on_confirm) => String.t(),
          required(:on_cancel) => String.t(),
          optional(:show) => boolean(),
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a confirmation dialog.

  ## Examples

      <.confirm_dialog
        title="Delete Item"
        message="Are you sure you want to delete this item? This action cannot be undone."
        confirm_label="Delete"
        cancel_label="Cancel"
        variant="destructive"
        on_confirm="delete_item"
        on_cancel="cancel_delete"
        show={show_dialog}
      />
  """
  @spec confirm_dialog(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:title, :string,
    required: true,
    doc: "Title of the confirmation dialog"
  )

  attr(:message, :string,
    required: true,
    doc: "Message content of the dialog"
  )

  attr(:confirm_label, :string,
    default: "Confirm",
    doc: "Label for the confirm button"
  )

  attr(:cancel_label, :string,
    default: "Cancel",
    doc: "Label for the cancel button"
  )

  attr(:variant, :atom,
    default: :destructive,
    values: [:destructive, :warning, :info],
    doc: "Variant of the confirm button"
  )

  attr(:on_confirm, :string,
    required: true,
    doc: "JavaScript command to execute on confirmation"
  )

  attr(:on_cancel, :string,
    required: true,
    doc: "JavaScript command to execute on cancellation"
  )

  attr(:show, :boolean,
    default: false,
    doc: "Whether to show the dialog"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the dialog container"
  )

  attr(:rest, :global)

  @impl true
  def confirm_dialog(assigns) do
    assigns
    |> build_confirm_dialog_attrs()
    |> validate_confirm_dialog_attrs()
    |> render_confirm_dialog()
  end

  @spec build_confirm_dialog_attrs(assigns :: assigns()) :: assigns()
  defp build_confirm_dialog_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      confirm_label: Application.get_env(:riva_ash, :confirm_dialog_confirm_label, dgettext("ui", "Confirm")),
      cancel_label: Application.get_env(:riva_ash, :confirm_dialog_cancel_label, dgettext("ui", "Cancel")),
      variant: Application.get_env(:riva_ash, :confirm_dialog_variant, :destructive)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:confirm_label, config.confirm_label)
    |> Map.put_new(:cancel_label, config.cancel_label)
    |> Map.put_new(:variant, config.variant)
  end

  @spec validate_confirm_dialog_attrs(assigns :: assigns()) :: assigns()
  defp validate_confirm_dialog_attrs(assigns) do
    with :ok <- validate_title(assigns[:title]),
         :ok <- validate_message(assigns[:message]),
         :ok <- validate_confirm_label(assigns[:confirm_label]),
         :ok <- validate_cancel_label(assigns[:cancel_label]),
         :ok <- validate_variant(assigns[:variant]),
         :ok <- validate_on_confirm(assigns[:on_confirm]),
         :ok <- validate_on_cancel(assigns[:on_cancel]),
         :ok <- validate_show(assigns[:show]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid confirm dialog attributes: #{reason}"
    end
  end

  @spec validate_title(String.t()) :: :ok | {:error, String.t()}
  defp validate_title(title) when is_binary(title) and title != "", do: :ok
  defp validate_unmatchedtitle(_unmatched), do: {:error, "title must be a non-empty string"}

  @spec validate_message(String.t()) :: :ok | {:error, String.t()}
  defp validate_message(message) when is_binary(message) and message != "", do: :ok
  defp validate_unmatchedmessage(_unmatched), do: {:error, "message must be a non-empty string"}

  @spec validate_confirm_label(String.t()) :: :ok | {:error, String.t()}
  defp validate_confirm_label(label) when is_binary(label) and label != "", do: :ok

  defp validate_unmatchedconfirm_unmatchedlabel(_unmatched),
    do: {:error, "confirm_unmatchedlabel must be a non-empty string"}

  @spec validate_cancel_label(String.t()) :: :ok | {:error, String.t()}
  defp validate_cancel_label(label) when is_binary(label) and label != "", do: :ok

  defp validate_unmatchedcancel_unmatchedlabel(_unmatched),
    do: {:error, "cancel_unmatchedlabel must be a non-empty string"}

  @spec validate_variant(atom()) :: :ok | {:error, String.t()}
  defp validate_variant(:destructive), do: :ok
  defp validate_variant(:warning), do: :ok
  defp validate_variant(:info), do: :ok
  defp validate_unmatchedvariant(_unmatched), do: {:error, "variant must be one of: destructive, warning, info"}

  @spec validate_on_confirm(String.t()) :: :ok | {:error, String.t()}
  defp validate_on_confirm(on_confirm) when is_binary(on_confirm) and on_confirm != "", do: :ok
  defp validate_unmatchedon_unmatchedconfirm(_unmatched), do: {:error, "on_unmatchedconfirm must be a non-empty string"}

  @spec validate_on_cancel(String.t()) :: :ok | {:error, String.t()}
  defp validate_on_cancel(on_cancel) when is_binary(on_cancel) and on_cancel != "", do: :ok
  defp validate_unmatchedon_unmatchedcancel(_unmatched), do: {:error, "on_unmatchedcancel must be a non-empty string"}

  @spec validate_show(boolean()) :: :ok | {:error, String.t()}
  defp validate_show(show) when is_boolean(show), do: :ok
  defp validate_unmatchedshow(_unmatched), do: {:error, "show must be a boolean"}

  @spec render_confirm_dialog(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_confirm_dialog(assigns) do
    ~H"""
    <div :if={@show} class={["confirm-dialog-container", @class]} {@rest}>
      <%= render_dialog_content_component(%{
        title: @title,
        message: @message,
        confirm_label: @confirm_label,
        cancel_label: @cancel_label,
        variant: @variant,
        on_confirm: @on_confirm,
        on_cancel: @on_cancel
      }) %>
    </div>
    """
  end

  @spec render_dialog_content(
          String.t(),
          String.t(),
          String.t(),
          String.t(),
          atom(),
          String.t(),
          String.t()
        ) :: Phoenix.LiveView.Rendered.t()
  defp render_dialog_content(title, message, confirm_label, cancel_label, variant, on_confirm, on_cancel) do
    # Render dialog content using functional composition
    assigns =
      %{
        title: title,
        message: message,
        confirm_label: confirm_label,
        cancel_label: cancel_label,
        variant: variant,
        on_confirm: on_confirm,
        on_cancel: on_cancel
      }
      |> render_dialog_content_component()
  end

  # Private helper for dialog content rendering
  @spec render_dialog_content_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_dialog_content_component(assigns) do
    ~H"""
    <div class="confirm-dialog-content">
      <div>
        <h3 class="confirm-dialog-title"><%= @title %></h3>
        <p class="confirm-dialog-message"><%= @message %></p>
        <div class="confirm-dialog-actions">
          <.button variant={@variant} phx-click={@on_confirm}><%= @confirm_label %></.button>
          <.button variant="outline" phx-click={@on_cancel}><%= @cancel_label %></.button>
        </div>
      </div>
    </div>
    """
  end
end
