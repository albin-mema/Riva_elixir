defmodule Storybook.Molecules.ConfirmDialog do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Molecules.ConfirmDialog.confirm_dialog/1

  def doc do
    """
    # ConfirmDialog

    Confirmation dialog component for destructive actions.

    ## Features

    - Modal dialog for confirming actions
    - Configurable title and message
    - Customizable confirm/cancel labels
    - Different variants (destructive, warning, info)
    - Custom CSS classes
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Confirm Action",
          message: "Are you sure you want to perform this action?",
          on_confirm: "confirm_action",
          on_cancel: "cancel_action",
          show: true
        }
      },
      %Variation{
        id: :destructive,
        attributes: %{
          title: "Delete Item",
          message: "This action cannot be undone. Are you sure you want to delete this item?",
          confirm_label: "Delete",
          cancel_label: "Cancel",
          variant: "destructive",
          on_confirm: "delete_item",
          on_cancel: "cancel_delete",
          show: true
        }
      },
      %Variation{
        id: :warning,
        attributes: %{
          title: "Warning",
          message: "This action may have unintended consequences. Please proceed with caution.",
          confirm_label: "Proceed",
          cancel_label: "Go Back",
          variant: "warning",
          on_confirm: "proceed_action",
          on_cancel: "go_back",
          show: true
        }
      },
      %Variation{
        id: :info,
        attributes: %{
          title: "Information",
          message: "Please confirm that you have reviewed all the information before proceeding.",
          confirm_label: "Continue",
          cancel_label: "Review",
          variant: "info",
          on_confirm: "continue_action",
          on_cancel: "review_info",
          show: true
        }
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :with_custom_styling,
        description: "Confirm dialog with custom CSS classes",
        template: """
        <.confirm_dialog
          title="Custom Style"
          message="This dialog has custom styling applied."
          on_confirm="custom_confirm"
          on_cancel="custom_cancel"
          show={true}
          class="bg-blue-50 border-blue-200"
        />
        """
      }
    ]
  end
end
