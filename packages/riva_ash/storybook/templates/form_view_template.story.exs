defmodule Storybook.Templates.FormViewTemplate do
  use PhoenixStorybook.Story, :component

  def function, do: &RivaAshWeb.Components.Templates.FormViewTemplate.form_view_template/1

  def doc do
    """
    # FormViewTemplate

    Form page template with validation and actions.

    ## Features

    - Page header with title and description
    - Progress indicator for multi-step forms
    - Form content area
    - Sidebar content area
    - Action buttons
    - Breadcrumb support
    """
  end

  def variations do
    [
      %Variation{
        id: :default,
        attributes: %{
          title: "Create New Reservation",
          description: "Fill in the details to create a new reservation",
          form_title: "Reservation Details",
          breadcrumbs: [
            %{label: "Home", href: "/"},
            %{label: "Reservations", href: "/reservations"},
            %{label: "Create", current: true}
          ]
        },
        slots: [
          """
          <:actions>
            <.button variant="outline">
              <.icon name={:x} class="mr-2 h-4 w-4" />
              Cancel
            </.button>
            <.button variant="default">
              <.icon name={:save} class="mr-2 h-4 w-4" />
              Save
            </.button>
          </:actions>
          """,
          """
          <:form_content>
            <div class="space-y-6">
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <.input label="First Name" placeholder="Enter first name" />
                <.input label="Last Name" placeholder="Enter last name" />
              </div>
              <.input label="Email" type="email" placeholder="Enter email address" />
              <.input label="Phone" type="tel" placeholder="Enter phone number" />
              <.select label="Resource" options={["Conference Room A", "Conference Room B", "Auditorium"]} />
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <.date_picker label="Start Date" />
                <.time_picker label="Start Time" />
              </div>
              <.textarea label="Special Requirements" placeholder="Any special requirements for this reservation" />
            </div>
          </:form_content>
          """,
          """
          <:sidebar_content>
            <.card>
              <:header>
                <h3 class="text-lg font-semibold">Reservation Tips</h3>
              </:header>
              <:body>
                <ul class="space-y-2 text-sm">
                  <li class="flex items-start">
                    <.icon name={:check_circle} class="h-4 w-4 text-green-500 mr-2 mt-0.5" />
                    <span>Book at least 24 hours in advance</span>
                  </li>
                  <li class="flex items-start">
                    <.icon name={:check_circle} class="h-4 w-4 text-green-500 mr-2 mt-0.5" />
                    <span>Check availability before booking</span>
                  </li>
                  <li class="flex items-start">
                    <.icon name={:check_circle} class="h-4 w-4 text-green-500 mr-2 mt-0.5" />
                    <span>Include special requirements</span>
                  </li>
                </ul>
              </:body>
            </.card>
          </:sidebar_content>
          """
        ]
      },
      %Variation{
        id: :with_progress,
        attributes: %{
          title: "Multi-step Form",
          description: "Complete all steps to finish the process",
          form_title: "Step 1: Personal Information",
          show_progress: true,
          current_step: 1,
          total_steps: 3
        },
        slots: [
          """
          <:actions>
            <.button variant="outline" disabled>
              Previous
            </.button>
            <.button variant="default">
              Next
            </.button>
          </:actions>
          """,
          """
          <:form_content>
            <div class="space-y-6">
              <.input label="First Name" placeholder="Enter first name" />
              <.input label="Last Name" placeholder="Enter last name" />
              <.input label="Email" type="email" placeholder="Enter email address" />
              <.input label="Phone" type="tel" placeholder="Enter phone number" />
            </div>
          </:form_content>
          """
        ]
      },
      %Variation{
        id: :step_two,
        attributes: %{
          title: "Multi-step Form",
          form_title: "Step 2: Reservation Details",
          show_progress: true,
          current_step: 2,
          total_steps: 3
        },
        slots: [
          """
          <:actions>
            <.button variant="outline">
              Previous
            </.button>
            <.button variant="default">
              Next
            </.button>
          </:actions>
          """,
          """
          <:form_content>
            <div class="space-y-6">
              <.select label="Resource" options={["Conference Room A", "Conference Room B", "Auditorium"]} />
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <.date_picker label="Start Date" />
                <.time_picker label="Start Time" />
              </div>
              <div class="grid grid-cols-1 md:grid-cols-2 gap-6">
                <.date_picker label="End Date" />
                <.time_picker label="End Time" />
              </div>
              <.textarea label="Special Requirements" placeholder="Any special requirements for this reservation" />
            </div>
          </:form_content>
          """
        ]
      }
    ]
  end

  def examples do
    [
      %Example{
        id: :without_sidebar,
        description: "Form view template without sidebar",
        attributes: %{
          title: "Simple Form",
          description: "A basic form without sidebar"
        },
        template: """
        <.form_view_template title="Simple Form" description="A basic form without sidebar">
          <:actions>
            <.button variant="default">Submit</.button>
          </:actions>
          <:form_content>
            <div class="space-y-4">
              <.input label="Name" placeholder="Enter your name" />
              <.input label="Email" type="email" placeholder="Enter your email" />
            </div>
          </:form_content>
        </.form_view_template>
        """
      },
      %Example{
        id: :with_custom_class,
        description: "Form view template with custom CSS classes",
        attributes: %{
          title: "Styled Form",
          class: "bg-gray-50 p-4"
        },
        template: """
        <.form_view_template title="Styled Form" class="bg-gray-50 p-4">
          <:form_content>
            <div class="bg-white p-6 rounded-lg shadow">
              <p>Custom styled form content</p>
            </div>
          </:form_content>
        </.form_view_template>
        """
      }
    ]
  end
end
