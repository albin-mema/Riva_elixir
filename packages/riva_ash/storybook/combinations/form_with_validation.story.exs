defmodule RivaAshWeb.Storybook.Combinations.FormWithValidation do
  use PhoenixStorybook.Story, :component
  use RivaAshWeb.Components.AtomicComponents

  def function, do: &form_with_validation/1

  def form_with_validation(assigns) do
    ~H"""
    <div class="p-6 max-w-md mx-auto">
      <.card variant="elevated">
        <:body>
          <.form_field field={:email} label="Email" errors={@form.errors}>
            <.input
              type="email"
              value={@form.values[:email]}
              name="email"
              class={if @form.errors[:email], do: "border-destructive", else: ""}
            />
          </.form_field>
          <div class="mt-4">
            <.button
              variant={if @form.errors == [], do: "primary", else: "secondary"}
              disabled={@form.errors != []}
            >
              Save Changes
            </.button>
          </div>
        </:body>
      </.card>
    </div>
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          form: %{
            errors: [],
            values: %{email: "user@example.com"}
          }
        }
      },
      %Variation{
        id: :with_errors,
        attributes: %{
          form: %{
            errors: [email: {"can't be blank", [validation: :required]}],
            values: %{email: ""}
          }
        }
      }
    ]
  end
end
