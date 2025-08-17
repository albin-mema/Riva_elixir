defmodule RivaAsh.ComboboxStories do
  use Phoenix.Component
  import RivaAsh.Components.UI.Combobox
  import RivaAsh.Components.UI.Form
  import RivaAsh.Components.UI.Card

  alias RivaAsh.Components.UI.{Form, Combobox}

  def basic_typeahead(assigns) do
    ~H"""
    <.card>
      <.card_content>
        <.combobox
          options={[
            %{id: "1", label: "Apple"},
            %{id: "2", label: "Banana"},
            %{id: "3", label: "Cherry"},
            %{id: "4", label: "Date"},
            %{id: "5", label: "Elderberry"}
          ]}
          option_label={& &1.label}
          option_key={& &1.id}
        />
      </.card_content>
    </.card>
    """
  end

  def async_loading(assigns) do
    ~H"""
    <.card>
      <.card_content>
        <.combobox
          options={@async_options}
          loading={@loading}
          empty={~H{<div class="px-4 py-2 text-muted-foreground text-center">Type to search...</div>}}
        />
      </.card_content>
    </.card>
    """
  end

  def keyboard_navigation(assigns) do
    ~H"""
    <.card>
      <.card_content>
        <.combobox
          options={[
            %{id: "1", label: "Option 1"},
            %{id: "2", label: "Option 2"},
            %{id: "3", label: "Option 3"},
            %{id: "4", label: "Option 4"},
            %{id: "5", label: "Option 5"}
          ]}
          open={true}
          active_index={@active_index}
        />
      </.card_content>
    </.card>
    """
  end

  def error_states(assigns) do
    ~H"""
    <.card>
      <.card_content>
        <.combobox
          options={[
            %{id: "1", label: "Valid Option"},
            %{id: "2", label: "Invalid Option"}
          ]}
          validate_selection={&(&1.id != "2")}
          error={@error}
        />
      </.card_content>
    </.card>
    """
  end

  def form_integration(assigns) do
    ~H"""
    <.card>
      <.card_content>
        <.form for={:form} phx-change="validate" phx-submit="save">
          <.combobox
            field={:category}
            options={[
              %{id: "1", label: "Business"},
              %{id: "2", label: "Personal"},
              %{id: "3", label: "Other"}
            ]}
          />
          <.button type="submit">Submit</.button>
        </.form>
      </.card_content>
    </.card>
    """
  end

  def mobile_viewport(assigns) do
    ~H"""
    <div class="mx-auto max-w-xs">
      <.combobox
        options={[
          %{id: "1", label: "Option 1"},
          %{id: "2", label: "Option 2"},
          %{id: "3", label: "Option 3"}
        ]}
      />
    </div>
    """
  end
end
