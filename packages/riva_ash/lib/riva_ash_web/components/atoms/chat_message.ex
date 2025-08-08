alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Atoms.ChatMessage do
  @moduledoc """
  Chat message atom component.

  Displays a single chat message with sender info, content, and timestamp.
  Supports different message types and alignment based on sender.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Avatar
  import RivaAshWeb.Components.Atoms.Text

  @doc """
  Renders a chat message.

  ## Examples

      <.chat_message 
        content="Hello everyone!" 
        sender_name="John Doe"
        timestamp={~N[2024-01-15 10:30:00]}
        is_own_message={false}
      />

      <.chat_message 
        content="Thanks for the update" 
        sender_name="You"
        timestamp={~N[2024-01-15 10:31:00]}
        is_own_message={true}
      />
  """
  attr :content, :string, required: true, doc: "The message content"
  attr :sender_name, :string, required: true, doc: "Name of the message sender"
  attr :sender_avatar, :string, default: nil, doc: "Avatar URL for the sender"
  attr :timestamp, :any, required: true, doc: "Message timestamp"
  attr :is_own_message, :boolean, default: false, doc: "Whether this message is from the current user"
  attr :message_type, :string, default: "text", doc: "Type of message (text, system, etc.)"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def chat_message(assigns) do
    ~H"""
    <div class={[
      "flex mb-3",
      if(@is_own_message, do: "justify-end", else: "justify-start"),
      @class
    ]} {@rest}>
      <div class={[
        "max-w-xs lg:max-w-md px-3 py-2 rounded-lg relative",
        message_bubble_classes(@is_own_message, @message_type)
      ]}>
        <%= if not @is_own_message do %>
          <div class="flex items-center mb-1">
            <%= if @sender_avatar do %>
              <.avatar src={@sender_avatar} size="xs" class="mr-2" />
            <% end %>
            <.text size="xs" weight="semibold" class="text-gray-700">
              <%= @sender_name %>
            </.text>
          </div>
        <% end %>
        
        <div class={[
          "text-sm leading-relaxed",
          if(@is_own_message, do: "text-white", else: "text-gray-900")
        ]}>
          <%= @content %>
        </div>
        
        <div class={[
          "text-xs mt-1 opacity-75",
          if(@is_own_message, do: "text-white", else: "text-gray-500")
        ]}>
          <%= format_timestamp(@timestamp) %>
        </div>
      </div>
    </div>
    """
  end

  # Private functions

  defp message_bubble_classes(is_own_message, message_type) do
    base_classes = ["shadow-sm"]

    type_classes =
      case message_type do
        "system" -> ["bg-gray-100", "text-gray-600", "italic"]
        "error" -> ["bg-red-100", "text-red-800", "border", "border-red-200"]
        _ -> []
      end

    alignment_classes =
      if is_own_message do
        ["bg-blue-500", "text-white", "rounded-br-sm"]
      else
        ["bg-gray-200", "text-gray-900", "rounded-bl-sm"]
      end

    base_classes ++ type_classes ++ alignment_classes
  end

  defp format_timestamp(timestamp) do
    case timestamp do
      %NaiveDateTime{} ->
        Calendar.strftime(timestamp, "%I:%M %p")

      %DateTime{} ->
        Calendar.strftime(timestamp, "%I:%M %p")

      _ ->
        "Now"
    end
  end
end
