alias RivaAshWeb.Components.Molecules, as: Molecules
alias RivaAshWeb.Components.Atoms, as: Atoms
alias Phoenix.LiveView.Rendered, as: Rendered

defmodule RivaAshWeb.Components.Molecules.NotificationToast do
  @moduledoc """
  Toast notification component for temporary messages.

  Provides a configurable toast notification system with different types,
  positions, durations, and dismiss functionality.

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
  import RivaAshWeb.Components.Atoms.Icon
  import RivaAshWeb.Components.Atoms.Button

  @type notification_config :: %{
          title: String.t() | nil,
          message: String.t(),
          type: notification_type(),
          duration: integer(),
          dismissible: boolean(),
          show: boolean(),
          position: position(),
          on_dismiss: String.t() | nil
        }
  @type notification_type :: :success | :error | :warning | :info
  @type position :: :top_left | :top_right | :bottom_left | :bottom_right
  @type assigns :: %{
          optional(:title) => String.t() | nil,
          required(:message) => String.t(),
          optional(:type) => notification_type(),
          optional(:duration) => integer(),
          optional(:dismissible) => boolean(),
          optional(:show) => boolean(),
          optional(:position) => position(),
          optional(:on_dismiss) => String.t() | nil,
          optional(:class) => String.t(),
          optional(:rest) => map()
        }

  @doc """
  Renders a toast notification.

  ## Examples

      <.notification_toast
        title="Success"
        message="Your changes have been saved successfully."
        type="success"
        duration={3000}
        dismissible={true}
        show={true}
        position="top-right"
        on_dismiss="dismiss_notification"
      />
  """
  @spec notification_toast(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  attr(:title, :string,
    default: nil,
    doc: "Title of the notification"
  )

  attr(:message, :string,
    required: true,
    doc: "Message content of the notification"
  )

  attr(:type, :atom,
    default: :info,
    values: [:success, :error, :warning, :info],
    doc: "Type of the notification"
  )

  attr(:duration, :integer,
    default: 5000,
    doc: "Duration in milliseconds before the toast auto-dismisses"
  )

  attr(:dismissible, :boolean,
    default: true,
    doc: "Whether the toast can be manually dismissed"
  )

  attr(:show, :boolean,
    default: false,
    doc: "Whether to show the toast"
  )

  attr(:position, :atom,
    default: :top_right,
    values: [:top_left, :top_right, :bottom_left, :bottom_right],
    doc: "Position of the toast on screen"
  )

  attr(:on_dismiss, :string,
    default: nil,
    doc: "JavaScript command to execute when the toast is dismissed"
  )

  attr(:class, :string,
    default: "",
    doc: "Additional CSS classes for the container"
  )

  attr(:rest, :global)

  @impl true
  def notification_toast(assigns) do
    assigns
    |> build_notification_toast_attrs()
    |> validate_notification_toast_attrs()
    |> render_notification_toast()
  end

  @spec build_notification_toast_attrs(assigns :: assigns()) :: assigns()
  defp build_notification_toast_attrs(assigns) do
    # Extract configuration with defaults using functional pattern
    config = %{
      type: Application.get_env(:riva_ash, :notification_toast_type, :info),
      duration: Application.get_env(:riva_ash, :notification_toast_duration, 5000),
      dismissible: Application.get_env(:riva_ash, :notification_toast_dismissible, true),
      position: Application.get_env(:riva_ash, :notification_toast_position, :top_right)
    }

    # Immutably update assigns with new values using pipeline
    assigns
    |> Map.put_new(:type, config.type)
    |> Map.put_new(:duration, config.duration)
    |> Map.put_new(:dismissible, config.dismissible)
    |> Map.put_new(:position, config.position)
  end

  @spec validate_notification_toast_attrs(assigns :: assigns()) :: assigns()
  defp validate_notification_toast_attrs(assigns) do
    with :ok <- validate_title(assigns[:title]),
         :ok <- validate_message(assigns[:message]),
         :ok <- validate_type(assigns[:type]),
         :ok <- validate_duration(assigns[:duration]),
         :ok <- validate_dismissible(assigns[:dismissible]),
         :ok <- validate_show(assigns[:show]),
         :ok <- validate_position(assigns[:position]),
         :ok <- validate_on_dismiss(assigns[:on_dismiss]),
         :ok <- validate_class(assigns[:class]) do
      assigns
    else
      {:error, reason} -> raise ArgumentError, "Invalid notification toast attributes: #{reason}"
    end
  end

  @spec validate_title(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_title(nil), do: :ok
  defp validate_title(title) when is_binary(title) and title != "", do: :ok
  defp validate_unmatchedtitle(_unmatched), do: {:error, "title must be a non-empty string or nil"}

  @spec validate_message(String.t()) :: :ok | {:error, String.t()}
  defp validate_message(message) when is_binary(message) and message != "", do: :ok
  defp validate_unmatchedmessage(_unmatched), do: {:error, "message must be a non-empty string"}

  @spec validate_type(notification_type()) :: :ok | {:error, String.t()}
  defp validate_type(:success), do: :ok
  defp validate_type(:error), do: :ok
  defp validate_type(:warning), do: :ok
  defp validate_type(:info), do: :ok
  defp validate_unmatchedtype(_unmatched), do: {:error, "type must be one of: success, error, warning, info"}

  @spec validate_duration(integer()) :: :ok | {:error, String.t()}
  defp validate_duration(duration) when is_integer(duration) and duration > 0, do: :ok
  defp validate_unmatchedduration(_unmatched), do: {:error, "duration must be a positive integer"}

  @spec validate_dismissible(boolean()) :: :ok | {:error, String.t()}
  defp validate_dismissible(dismissible) when is_boolean(dismissible), do: :ok
  defp validate_unmatcheddismissible(_unmatched), do: {:error, "dismissible must be a boolean"}

  @spec validate_show(boolean()) :: :ok | {:error, String.t()}
  defp validate_show(show) when is_boolean(show), do: :ok
  defp validate_unmatchedshow(_unmatched), do: {:error, "show must be a boolean"}

  @spec validate_position(position()) :: :ok | {:error, String.t()}
  defp validate_position(:top_left), do: :ok
  defp validate_position(:top_right), do: :ok
  defp validate_position(:bottom_left), do: :ok
  defp validate_position(:bottom_right), do: :ok

  defp validate_unmatchedposition(_unmatched),
    do:
      {:error,
       "position must be one of: top_unmatchedleft, top_unmatchedright, bottom_unmatchedleft, bottom_unmatchedright"}

  @spec validate_on_dismiss(String.t() | nil) :: :ok | {:error, String.t()}
  defp validate_on_dismiss(nil), do: :ok
  defp validate_on_dismiss(on_dismiss) when is_binary(on_dismiss) and on_dismiss != "", do: :ok

  defp validate_unmatchedon_unmatcheddismiss(_unmatched),
    do: {:error, "on_unmatcheddismiss must be a non-empty string or nil"}

  @spec validate_class(String.t()) :: :ok | {:error, String.t()}
  defp validate_class(class) when is_binary(class), do: :ok
  defp validate_unmatchedclass(_unmatched), do: {:error, "class must be a string"}

  @spec render_notification_toast(assigns :: assigns()) :: Phoenix.LiveView.Rendered.t()
  defp render_notification_toast(assigns) do
    # Render notification toast using functional composition
    assigns
    |> Map.put_new(:container_class, build_container_class(assigns.class, assigns.position))
    |> render_notification_toast_component()
  end

  # Private helper for notification toast rendering
  @spec render_notification_toast_component(assigns :: map()) :: Phoenix.LiveView.Rendered.t()
  defp render_notification_toast_component(assigns) do
    ~H"""
    <div :if={@show} class={["notification-toast-container fixed z-50 max-w-sm rounded-lg border p-4 shadow-lg", @container_class]} {@rest}>
      <.render_content
        title={@title}
        message={@message}
        type={@type}
        dismissible={@dismissible}
        on_dismiss={@on_dismiss}
      />
    </div>
    """
  end

  # Helper function to build container classes
  @spec build_container_class(String.t(), position()) :: String.t()
  defp build_container_class(class, position) do
    classes =
      [
        class,
        build_position_classes(position)
      ]
      # Remove empty strings
      |> Enum.reject(&(&1 == ""))
      # Join with spaces
      |> Enum.join(" ")

    classes
  end

  @spec render_content(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:title, :string, default: nil)
  attr(:message, :string, required: true)
  attr(:type, :atom, default: :info, values: [:success, :error, :warning, :info])
  attr(:dismissible, :boolean, default: true)
  attr(:on_dismiss, :string, default: nil)

  defp render_content(assigns) do
    ~H"""
    <div class="notification-toast-content flex items-start justify-between">
      <div class="notification-toast-body flex items-center gap-3">
        <.icon name={icon_for_type(@type)} />
        <div>
          <h4 :if={@title}><%= @title %></h4>
          <p><%= @message %></p>
        </div>
      </div>
      <%= if @dismissible do %>
        <.render_dismiss_button on_dismiss={@on_dismiss} />
      <% end %>
    </div>
    """
  end

  @spec render_dismiss_button(map()) :: Phoenix.LiveView.Rendered.t()
  attr(:on_dismiss, :string, default: nil)

  defp render_dismiss_button(assigns) do
    ~H"""
    <.button variant="ghost" size="sm" phx-click={@on_dismiss}>
      <.icon name={:x_mark} />
    </.button>
    """
  end

  @spec build_position_classes(position()) :: String.t()
  defp build_position_classes(:top_left), do: "top-4 left-4"
  defp build_position_classes(:top_right), do: "top-4 right-4"
  defp build_position_classes(:bottom_left), do: "bottom-4 left-4"
  defp build_position_classes(:bottom_right), do: "bottom-4 right-4"

  @spec icon_for_type(notification_type()) :: atom()
  defp icon_for_type(:success), do: :check_circle
  defp icon_for_type(:error), do: :x_circle
  defp icon_for_type(:warning), do: :alert_triangle
  defp icon_for_type(:info), do: :information_circle
end
