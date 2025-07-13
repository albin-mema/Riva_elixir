defmodule Storybook.Atoms.Select do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Atoms.Select.select/1

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{},
        slots: [
          """
          <option value="">Choose an option</option>
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
          <option value="option3">Option 3</option>
          """
        ]
      },
      %Variation{
        id: :with_selected,
        attributes: %{
          value: "option2"
        },
        slots: [
          """
          <option value="">Choose an option</option>
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
          <option value="option3">Option 3</option>
          """
        ]
      },
      %Variation{
        id: :disabled,
        attributes: %{
          disabled: true
        },
        slots: [
          """
          <option value="">Choose an option</option>
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
          <option value="option3">Option 3</option>
          """
        ]
      },
      %Variation{
        id: :multiple,
        attributes: %{
          multiple: true
        },
        slots: [
          """
          <option value="option1">Option 1</option>
          <option value="option2">Option 2</option>
          <option value="option3">Option 3</option>
          <option value="option4">Option 4</option>
          <option value="option5">Option 5</option>
          """
        ]
      },
      %Variation{
        id: :sizes,
        template: """
        <div class="space-y-4">
          <.select size="sm">
            <option value="">Small select</option>
            <option value="1">Option 1</option>
            <option value="2">Option 2</option>
          </.select>
          <.select size="md">
            <option value="">Medium select</option>
            <option value="1">Option 1</option>
            <option value="2">Option 2</option>
          </.select>
          <.select size="lg">
            <option value="">Large select</option>
            <option value="1">Option 1</option>
            <option value="2">Option 2</option>
          </.select>
        </div>
        """
      },
      %Variation{
        id: :variants,
        template: """
        <div class="space-y-4">
          <.select variant="default">
            <option value="">Default variant</option>
            <option value="1">Option 1</option>
          </.select>
          <.select variant="error">
            <option value="">Error variant</option>
            <option value="1">Option 1</option>
          </.select>
          <.select variant="success" value="1">
            <option value="">Success variant</option>
            <option value="1">Selected option</option>
          </.select>
        </div>
        """
      }
    ]
  end
end
