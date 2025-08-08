alias RivaAshWeb.Components.Atoms, as: Atoms

defmodule RivaAshWeb.Components.Atoms.ChatInput do
  @moduledoc """
  Chat input atom component.

  A specialized input field for chat messages with send button integration.
  Supports keyboard shortcuts and real-time message composition.
  """

  use Phoenix.Component
  import RivaAshWeb.Components.Atoms.Input
  import RivaAshWeb.Components.Atoms.Button

  @doc """
  Renders a chat input field with send button.

  ## Examples

      <.chat_input 
        value={@message} 
        placeholder="Type your message..."
        on_send="send_message"
        on_change="update_message"
      />

      <.chat_input 
        value={@message} 
        placeholder="Type your message..."
        on_send="send_message"
        on_change="update_message"
        disabled={@sending}
        loading={@sending}
      />
  """
  attr :value, :string, default: "", doc: "Current input value"
  attr :placeholder, :string, default: "Type your message...", doc: "Input placeholder text"
  attr :on_send, :string, required: true, doc: "Phoenix event for sending message"
  attr :on_change, :string, required: true, doc: "Phoenix event for input changes"
  attr :disabled, :boolean, default: false, doc: "Whether input is disabled"
  attr :loading, :boolean, default: false, doc: "Whether send button shows loading state"
  attr :max_length, :integer, default: 1000, doc: "Maximum message length"
  attr :size, :string, default: "default", doc: "Input size (sm, default, lg)"
  attr :class, :string, default: "", doc: "Additional CSS classes"
  attr :rest, :global

  def chat_input(assigns) do
    ~H"""
    <div class={["flex space-x-2", @class]} {@rest}>
      <form phx-submit={@on_send} class="flex-1 flex space-x-2">
        <.input
          type="text"
          name="message"
          value={@value}
          phx-change={@on_change}
          placeholder={@placeholder}
          disabled={@disabled}
          maxlength={@max_length}
          size={@size}
          class="flex-1"
          autocomplete="off"
          phx-hook="ChatInput"
        />
        
        <.button 
          type="submit" 
          variant="primary" 
          size={button_size(@size)}
          disabled={@disabled or String.trim(@value) == ""}
          loading={@loading}
          class="shrink-0"
        >
          <span class="sr-only">Send message</span>
          <%= if @loading do %>
            <svg class="w-4 h-4 animate-spin" fill="none" viewBox="0 0 24 24">
              <circle class="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" stroke-width="4"></circle>
              <path class="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
            </svg>
          <% else %>
            <svg class="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"></path>
            </svg>
          <% end %>
        </.button>
      </form>
      
      <%= if @max_length > 0 do %>
        <div class={[
          "text-xs text-gray-500 self-end pb-2 whitespace-nowrap",
          if(String.length(@value) > @max_length * 0.8, do: "text-orange-500"),
          if(String.length(@value) >= @max_length, do: "text-red-500")
        ]}>
          <%= String.length(@value) %>/<%= @max_length %>
        </div>
      <% end %>
    </div>
    """
  end

  # Private functions

  defp button_size("sm"), do: "sm"
  defp button_size("lg"), do: "lg"
  defp button_size(_), do: "default"
end
